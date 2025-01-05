open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unop (Opp, e1) ->
        (match type_expr e1 tenv with
        | TInt -> TInt
        | ty -> type_error ty TInt)
    | Unop (Not, e1) ->
        (match type_expr e1 tenv with
        | TBool -> TBool
        | ty -> type_error ty TBool)
    | Binop (op, e1, e2) ->
        (match op, type_expr e1 tenv, type_expr e2 tenv with
        | (Add | Sub | Mul | Div | Rem), TInt, TInt -> TInt
        | (Lt | Le | Gt | Ge), TInt, TInt -> TBool
        | (Eq | Neq), t1, t2 when t1 = t2 -> TBool
        | (And | Or), TBool, TBool -> TBool
        | _ -> error "Invalid binary operation or operand types")
    | Get (Var x) ->
        (try Env.find x tenv
         with Not_found -> error ("Variable not found: " ^ x))
    | Get (Field (e, field)) ->
        (match type_expr e tenv with
        | TClass cname ->
            let cls =
              try List.find (fun c -> c.class_name = cname) p.classes
              with Not_found -> error ("Class not found: " ^ cname)
            in
            (try List.assoc field cls.attributes
             with Not_found -> error ("Field not found: " ^ field))
        | ty -> type_error ty (TClass "object"))
    | This -> TClass "current" (* Suppose un contexte de classe. *)
    | New cname ->
        if List.exists (fun c -> c.class_name = cname) p.classes then TClass cname
        else error ("Class not found: " ^ cname)
    | NewCstr (cname, args) ->
        let cls =
          try List.find (fun c -> c.class_name = cname) p.classes
          with Not_found -> error ("Class not found: " ^ cname)
        in
        let cstr_params =
          match List.find_opt (fun m -> m.method_name = "constructor") cls.methods with
          | Some m -> List.map snd m.params
          | None -> []
        in
        if List.length cstr_params <> List.length args then
          error "Constructor argument count mismatch";
        List.iter2
          (fun param_type arg ->
            let arg_type = type_expr arg tenv in
            if param_type <> arg_type then
              type_error arg_type param_type)
          cstr_params args;
        TClass cname
    | MethCall (obj, mname, args) ->
        (match type_expr obj tenv with
        | TClass cname ->
            let cls =
              try List.find (fun c -> c.class_name = cname) p.classes
              with Not_found -> error ("Class not found: " ^ cname)
            in
            let method_ =
              try List.find (fun m -> m.method_name = mname) cls.methods
              with Not_found -> error ("Method not found: " ^ mname)
            in
            if List.length method_.params <> List.length args then
              error "Method argument count mismatch";
            List.iter2
              (fun (_, param_type) arg ->
                let arg_type = type_expr arg tenv in
                if param_type <> arg_type then
                  type_error arg_type param_type)
              method_.params args;
            method_.return
        | ty -> type_error ty (TClass "object"))
    | EArrayCreate (array_type, size_expr) ->
      (* Vérifiez que `size_expr` est un entier *)
      check size_expr TInt tenv;
      TArray array_type
      
    | EArrayGet (arr_expr, index_expr) ->
      (* Vérifiez que `arr_expr` est un tableau et que `index_expr` est un entier *)
      (match type_expr arr_expr tenv with
      | TArray inner_type ->
        check index_expr TInt tenv;
        inner_type
        | ty -> type_error ty (TArray TInt))  (* Erreur si ce n'est pas un tableau *)
    | EArraySet (arr_expr, index_expr, value_expr) ->
      (match type_expr arr_expr tenv with
      | TArray inner_type ->
        (* `index_expr` doit être un entier et `value_expr` doit correspondre au type du tableau *)
        check index_expr TInt tenv;
        check value_expr inner_type tenv;
        TVoid
      | ty -> type_error ty (TArray TInt))   
    | _ -> error "Unhandled expression case"

  and type_mem_access m tenv = match m with
    | Var x -> Env.find x tenv
    | Field (e, field) ->
        (match type_expr e tenv with
        | TClass cname ->
            let cls =
              try List.find (fun c -> c.class_name = cname) p.classes
              with Not_found -> error ("Class not found: " ^ cname)
            in
            (try List.assoc field cls.attributes
             with Not_found -> error ("Field not found: " ^ field))
        | ty -> type_error ty (TClass "object"))
    | ArrayAccess (arr_expr, index_expr) ->
      (match type_expr arr_expr tenv with
      | TArray elem_type ->
        (* Vérifiez que l'index est un entier *)
        check index_expr TInt tenv;
        elem_type
      | ty -> type_error ty (TArray TInt))

  and check_instr i ret tenv = match i with
    | Print e ->
      check e TInt tenv
    | Set (Var x, e) ->
      let tvar = Env.find x tenv in
      check e tvar tenv
    | Set (Field (obj, field), e) ->
      (match type_expr obj tenv with
    | TClass cname ->
      let cls =
        try List.find (fun c -> c.class_name = cname) p.classes
        with Not_found -> error ("Class not found: " ^ cname)
      in
      let tfield =
        try List.assoc field cls.attributes
        with Not_found -> error ("Field not found: " ^ field)
      in
      check e tfield tenv
      | ty -> type_error ty (TClass "object"))
    | Set (ArrayAccess (arr_expr, index_expr), value_expr) ->
      (* Vérifiez que arr_expr est un tableau *)
      (match type_expr arr_expr tenv with
      | TArray elem_type ->
        check index_expr TInt tenv;  (* L'index doit être un entier *)
        check value_expr elem_type tenv  (* La valeur doit correspondre au type du tableau *)
      | ty -> type_error ty (TArray TInt))
    | Set (Var x, EArrayCreate (arr_type, size_expr)) ->
      let tvar = Env.find x tenv in
      check size_expr TInt tenv;  (* La taille doit être un entier *)
      if tvar <> TArray arr_type then
        type_error tvar (TArray arr_type)
    (*| Set (Field (arr_expr, index_expr), value_expr) ->
      let index_expr_ast = Var index_expr in
      (* Vérifiez que arr_expr est un tableau imbriqué dans un champ *)
      (match type_expr arr_expr tenv with
      | TArray elem_type ->
            (* ** ** * * * *** * **)
        check index_expr_ast TInt tenv;  (* L'index doit être un entier *)
        check value_expr elem_type tenv;  (* La valeur doit correspondre au type des éléments du tableau *)
      | ty -> type_error ty (TArray TInt))*)
    | If (cond, then_seq, else_seq) ->
      check cond TBool tenv;
      check_seq then_seq ret tenv;
      check_seq else_seq ret tenv
    | While (cond, body) ->
      check cond TBool tenv;
      check_seq body ret tenv
    | Return e ->
      check e ret tenv
    | Expr e ->
      ignore (type_expr e tenv)
      | _ -> error "Unhandled expression case"
  

  

  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s

  in

  check_seq p.main TVoid tenv
