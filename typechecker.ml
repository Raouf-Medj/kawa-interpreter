
open Kawa

exception Error of string
exception ArrayError of string
exception VarNotFound of string

let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let check_eq_type expected actual =
  if expected <> actual then type_error  actual expected
let rec get_array_core_type = function
  | TArray t -> get_array_core_type t
  | t -> t

let add_env l tenv =
    List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec find_class classes cname =
    try List.find (fun c -> c.class_name = cname) classes
    with Not_found -> error ("Class not found: " ^ cname)

  and find_method cls mname =
    try List.find (fun m -> m.method_name = mname) cls.methods
    with Not_found -> error ("Method not found: " ^ mname)

  and check e typ tenv =
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
        | (Structeg| Structineg),t1, t2 when t1=t2 -> TBool
        | _ -> error "Invalid binary operation or operand types")
    
    | Get m -> type_mem_access m tenv

    (*| Get (Var x) -> (try Env.find x tenv with Not_found -> error ("Variable not found: " ^ x))
    | Get (Field (e, field)) ->
        (match type_expr e tenv with
        | TClass cname ->
            let cls = find_class p.classes cname in
            (try List.assoc field cls.attributes
            with Not_found -> error ("Field not found: " ^ field))
        | ty -> type_error ty (TClass "object"))*)
           
    (*| Get(ArrayAccess(arr_expr, index_expr))->TInt*)
        (*(match type_expr arr_expr tenv with
            | TArray elem_type ->
                check index_expr TInt tenv;
                elem_type
            | ty -> type_error ty (TArray TInt))*)
    | This -> 
        (try
          Env.find "this" tenv
        with Not_found ->
          error "Variable 'this' not found")
    | New cname ->
        let _ = find_class p.classes cname in
        TClass cname
    | NewCstr (cname, args) ->
        let cls = find_class p.classes cname in
        let cstr_params =
          match List.find_opt (fun m -> m.method_name = "constructor") cls.methods with
          | Some m -> List.map snd m.params
          | None -> error ("Constructor not defined in class: " ^ cname)
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
            let cls = find_class p.classes cname in
            let method_ = find_method cls mname in
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
    | EArrayCreate(t, n) ->
      let typed_n = List.map (fun x -> type_expr x tenv) n in
      List.iter (fun x -> check_eq_type TInt x) typed_n;
      let rec retu n = 
        if n == 1 then t
        else TArray (retu (n-1)) 
      in  TArray (retu (List.length n))
      | InstanceOf (expr, cname) ->
        let expr_type = type_expr expr tenv in
        (match expr_type with
         | TClass actual_cname ->
             (* Vérifiez si la classe existe *)
             let _ = find_class p.classes cname in
             (* Retourne TBool car `instanceof` retourne une valeur booléenne *)
             TBool
         | _ -> type_error expr_type (TClass "object"))
    



    
  (***************)
  and type_mem_access m tenv : typ =
    let rec reduce_dim t dims =
      match dims with
      | [] -> t
      | hd :: tl ->
          check_eq_type TInt (type_expr hd tenv);
          (match t with
           | TArray elem_type -> reduce_dim elem_type tl
           | _ -> error "Dimension mismatch: expected an array")
    in
    match m with
    | Var name -> (
        try Env.find name tenv
        with Not_found ->
          error ("Undeclared variable"))
    | Field (obj, field_name) ->
        let obj_type = type_expr obj tenv in
        (match obj_type with
         | TClass cname ->
             let cls = find_class p.classes cname in
             let rec find_field c =
               try List.assoc field_name c.attributes
               with Not_found ->
                 (match c.parent with
                  | Some parent_name ->
                      let parent_cls = find_class p.classes parent_name in
                      find_field parent_cls
                  | None ->
                      error ("Field " ^ field_name ^ " not declared in class " ^ cname))
             in
             find_field cls
         | ty -> type_error ty (TClass "object"))
    | ArrayAccess (name, indices) -> (
        try
          let arr_type = Env.find name tenv in
          try reduce_dim arr_type indices
          with Error msg -> error msg
        with Not_found ->
          error ("Undeclared variable: "))
    
  
  (******)

  and check_instr i ret tenv = 
    let rec is_subtype child parent =
      if child = parent then true
      else
        match find_class p.classes child  with
        | { parent = Some parent_name; _ } -> is_subtype parent_name parent
        | _ -> false
      in match i with
    | Print e ->
        let typed_e = type_expr e tenv in
        if typed_e == TInt || typed_e == TBool then () (*si on veut print d'autres trucs modifier ça en  list ou autre*)
        else type_error typed_e TInt
    | Set (Var x, e) ->
      let tvar = Env.find x tenv in
      let texpr = type_expr e tenv in
      if texpr <> tvar then
        match tvar, texpr with
        | TClass parent, TClass child when is_subtype child parent -> ()
        | _ -> type_error texpr tvar
      else
        ()

    (*| Set (Field (obj, field), e) ->
        (match type_expr obj tenv with
        | TClass cname ->
            let cls = find_class p.classes cname in
            let tfield =
              try List.assoc field cls.attributes
              with (try List.assoc field cls.parent.attributes
                    with Not_found -> error ("Field not found: " ^ field))
            in
            check e tfield tenv
        | ty -> type_error ty (TClass "object"))*)
        | Set (Field (obj, field), e) ->
          (match type_expr obj tenv with
          | TClass cname ->
              let rec find_field cls_name =
                let cls = find_class p.classes cls_name in
                match List.assoc_opt field cls.attributes with
                | Some tfield -> tfield
                | None -> (
                    match cls.parent with
                    | Some parent_name -> find_field parent_name
                    | None -> error ("Field " ^ field ^ " not found in class " ^ cls_name)
                  )
              in
              let tfield = find_field cname in
              check e tfield tenv
          | ty -> type_error ty (TClass "object"))
      
    | Set (ArrayAccess (arr_expr, index_expr), value_expr) -> 
      let tvar = type_mem_access (ArrayAccess (arr_expr, index_expr)) tenv in
      check value_expr tvar tenv
  
       (* (match type_expr arr_expr tenv with
        | TArray elem_type ->
            check index_expr TInt tenv;
            check value_expr elem_type tenv
        | ty -> type_error ty (TArray TInt))*)
    | If (cond, then_seq, else_seq) ->
        check cond TBool tenv;
        check_seq then_seq ret tenv;
        check_seq else_seq ret tenv
    | While (cond, body) ->
        check cond TBool tenv;
        check_seq body ret tenv
    | Return e -> check e ret tenv
    | Expr e -> ignore (type_expr e tenv)

  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s

  and check_method cls method_ tenv =
    let method_env = add_env (method_.params @ method_.locals) tenv in
    check_seq method_.code method_.return method_env

  and check_class cls tenv =
    let class_env = add_env cls.attributes tenv in
    let class_env_this = Env.add "this" (TClass(cls.class_name)) class_env in
    List.iter (fun method_ -> check_method cls method_ class_env_this) cls.methods

  in
  List.iter (fun cls -> check_class cls tenv) p.classes;
  check_seq p.main TVoid tenv
