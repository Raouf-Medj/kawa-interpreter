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

  let rec find_class classes cname =
    try List.find (fun c -> c.class_name = cname) classes
    with Not_found -> error ("Class not found: " ^ cname)

  and find_method cls mname =
    try List.find (fun m -> m.method_name = mname) cls.methods
    with Not_found -> error ("Method not found: " ^ mname)

  and check e typ tenv =
    let typ_e = type_expr e tenv in
    match typ_e, typ with
    | TClass cls1, TClass cls2 -> 
      if not (class_incluse p.classes cls1 cls2) 
        then error ("Class " ^ cls1 ^ " is not a subclass of " ^ cls2)
    | _ -> if typ_e <> typ then type_error typ_e typ

  and class_incluse classes cname1 cname2 =
    if cname1 = cname2 then true
    else
      let cls = find_class classes cname1 in
      match cls.parent with
      | Some parent -> class_incluse classes parent cname2
      | None -> false
  
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
      (match op with
      | Add | Sub | Mul | Div | Rem ->
          check e1 TInt tenv; check e2 TInt tenv; TInt
      | Lt | Le | Gt | Ge ->
          check e1 TInt tenv; check e2 TInt tenv; TBool
      | Eq | Neq ->
          let t1 = type_expr e1 tenv in
          let t2 = type_expr e2 tenv in
          if t1 <> t2 then type_error t2 t1;
          TBool
      | And | Or ->
          check e1 TBool tenv; check e2 TBool tenv; TBool)
    | Get (Var x) -> (try Env.find x tenv with Not_found -> error ("Variable not found: " ^ x))
    | Get (Field (e, field)) ->
        (match type_expr e tenv with
        | TClass cname ->
            let cls = find_class p.classes cname in
            find_field_type cls field
        | ty -> type_error ty (TClass "object"))
    | This -> 
      (try Env.find "this" tenv with Not_found -> error ("Unbound 'this' in the current context"))
    | New cname -> 
      let _ = find_class p.classes cname in TClass cname
    | NewCstr (cname, args) ->
        let cls = find_class p.classes cname in
        let cstr_params =
          match List.find_opt (fun m -> m.method_name = "constructor") cls.methods with
          | Some m -> List.map snd m.params
          | None -> error ("Constructor not defined in class: " ^ cname)
        in
        if List.length cstr_params <> List.length args then
          error "Constructor argument count mismatch";
        List.iter2 (fun param_type arg -> check arg param_type tenv) cstr_params args;
        TClass cname
    | MethCall (obj, mname, args) ->
        (match type_expr obj tenv with
        | TClass cname ->
            let cls = find_class p.classes cname in
            let method_ = find_method cls mname in
            if List.length method_.params <> List.length args then
              error "Method argument count mismatch";
            List.iter2 (fun (_, param_type) arg -> check arg param_type tenv) method_.params args;
            method_.return
        | ty -> type_error ty (TClass "object"))
    | InstanceOf (e, cname) ->
      (let _ = find_class p.classes cname in
      match type_expr e tenv with
      | TClass ty -> TBool
      | ty -> type_error ty (TClass "object"))
  
  and find_field_type cls field =
    try List.assoc field cls.attributes
    with Not_found ->
      match cls.parent with
      | Some parent_name ->
          let parent_cls = find_class p.classes parent_name in
          find_field_type parent_cls field
      | None -> error ("Field not found: " ^ field)
  
  and find_field_is_final cls field =
    try List.assoc field cls.is_attr_final
    with Not_found ->
      match cls.parent with
      | Some parent_name ->
          let parent_cls = find_class p.classes parent_name in
          find_field_is_final parent_cls field
      | None -> error ("Field not found: " ^ field)

  and check_instr i ret tenv mname = match i with
    | Print e -> (
      try check e TInt tenv 
      with exn ->
        try check e TBool tenv
        with exn -> check e TVoid tenv)
    | Set (Var x, e) ->
        let tvar = Env.find x tenv in
        check e tvar tenv
    | Set (Field (obj, field), e) ->
        (match type_expr obj tenv with
        | TClass cname ->
            let cls = find_class p.classes cname in
            let tfield = find_field_type cls field in
            let is_final_field = find_field_is_final cls field in
            if (is_final_field && not (String.equal mname "constructor")) then error ("Field " ^ field ^ " is final");
            check e tfield tenv
        | ty -> type_error ty (TClass "object"))
    | If (cond, then_seq, else_seq) ->
        check cond TBool tenv;
        check_seq then_seq ret tenv mname;
        check_seq else_seq ret tenv mname
    | While (cond, body) ->
        check cond TBool tenv;
        check_seq body ret tenv mname
    | Return e -> check e ret tenv
    | Expr e -> check e TVoid tenv

  and check_seq s ret tenv mname =
    List.iter (fun i -> check_instr i ret tenv mname) s

  and check_method cls method_ tenv =
    let method_env = add_env (method_.params @ method_.locals) tenv in
    check_seq method_.code method_.return method_env method_.method_name

  and check_class cls tenv =
    let class_env = add_env cls.attributes tenv in
    let class_env_this = Env.add "this" (TClass(cls.class_name)) class_env in
    List.iter (fun method_ -> check_method cls method_ class_env_this) cls.methods

  in
  List.iter (fun cls -> check_class cls tenv) p.classes;
  check_seq p.main TVoid tenv "main"
