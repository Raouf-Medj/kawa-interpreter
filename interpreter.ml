open Kawa

type value =
  | VInt of int
  | VBool of bool
  | VObj of obj
  | Null
and obj = {
  cls: string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let error s = raise (Error s)

let rec exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  (* Initialize global variables *)
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;


  let create_object cname =
    match List.find_opt (fun c -> c.class_name = cname) p.classes with
    | Some cls ->
        let fields = Hashtbl.create 16 in
        List.iter (fun (field, _) -> Hashtbl.add fields field (VInt 0)) cls.attributes;
        VObj { cls = cname; fields }
    | None -> error ("Class not found: " ^ cname)
  
  in
  let rec eval_expr ?(this=None) e env =
    match e with
    | Int n -> VInt n
    | Bool b -> VBool b
    | Unop (Opp, e1) ->
        (match eval_expr e1 env with
         | VInt n -> VInt (-n)
         | _ -> error "Unary '-' applied to non-integer")
    | Unop (Not, e1) ->
        (match eval_expr e1 env with
         | VBool b -> VBool (not b)
         | _ -> error "Unary 'not' applied to non-boolean")
    | Binop (op, e1, e2) ->
        let v1 = eval_expr e1 env in
        let v2 = eval_expr e2 env in
        eval_binop op v1 v2
    | Get (Var x) ->
        (try Hashtbl.find env x
         with Not_found -> error ("Variable not found: " ^ x))
    | Get (Field (e, field)) ->
      (match eval_expr e env with
      | VObj obj ->
         (try 
           let value = Hashtbl.find obj.fields field in
           (*Printf.eprintf "[DEBUG] Accessing field %s: %s\n" field
             (match value with
             | VInt n -> string_of_int n
             | Null -> "Null"
             | _ -> "Other");*)
            value (* Retournez la valeur après avoir trouvé le champ *)
          with Not_found -> error ("Field not found: " ^ field))
     | _ -> error "Field access on non-object")


    | New cname -> create_object cname

    
    | NewCstr (cname, args) ->
        let obj = create_object cname in
        let cls = find_class cname p.classes in
        let constructor =
          match List.find_opt (fun m -> m.method_name = "constructor") cls.methods with
          | Some cstr -> cstr
          | None -> error ("Constructor not found for class: " ^ cname)
        in
        let arg_values = List.map (fun a -> eval_expr  ~this a env) args in
        let local_env = add_params_to_env constructor.params arg_values env in
        ignore (exec_seq ~this:(Some obj) constructor.code local_env);
        obj
        

    
    | This ->
      (match this with
      | Some obj -> VObj obj
      | None -> error "Unbound 'this' in current context")
    | MethCall (obj_expr, mname, args) ->
      (match eval_expr ~this obj_expr env with
      | VObj obj -> call_method obj mname args env
      | _ -> error "Method call on non-object")      
    

  and eval_binop op v1 v2 =
    (*Printf.eprintf "[DEBUG] Binary operation %s: %s and %s\n"
      (match op with
       | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
       | Rem -> "%" | Lt -> "<" | Le -> "<=" | Gt -> ">"
       | Ge -> ">=" | Eq -> "==" | Neq -> "!="
       | And -> "&&" | Or -> "||")
      (match v1 with
       | VInt n -> string_of_int n
       | VBool b -> string_of_bool b
       | VObj _ -> "Object"
       | Null -> "Null")
      (match v2 with
       | VInt n -> string_of_int n
       | VBool b -> string_of_bool b
       | VObj _ -> "Object"
       | Null -> "Null");*)
    match op, v1, v2 with
    | Add, VInt n1, VInt n2 -> VInt (n1 + n2)
    | Sub, VInt n1, VInt n2 -> VInt (n1 - n2)
    | Mul, VInt n1, VInt n2 -> VInt (n1 * n2)
    | Div, VInt n1, VInt n2 -> VInt (n1 / n2)
    | Rem, VInt n1, VInt n2 -> VInt (n1 mod n2)
    | Lt, VInt n1, VInt n2 -> VBool (n1 < n2)
    | Le, VInt n1, VInt n2 -> VBool (n1 <= n2)
    | Gt, VInt n1, VInt n2 -> VBool (n1 > n2)
    | Ge, VInt n1, VInt n2 -> VBool (n1 >= n2)
    | Eq, v1, v2 -> VBool (v1 = v2)
    | Neq, v1, v2 -> VBool (v1 <> v2)
    | And, VBool b1, VBool b2 -> VBool (b1 && b2)
    | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
    | _ -> error "Invalid binary operation or operand types"
  
    and call_method obj mname args env =
      let cls = find_class obj.cls p.classes in
      let method_ =
        match List.find_opt (fun m -> m.method_name = mname) cls.methods with
        | Some m -> m
        | None -> error ("Method not found: " ^ mname)
      in
      let arg_values = List.map (fun a -> eval_expr a env) args in
      let local_env = add_params_to_env method_.params arg_values env in
      ignore (exec_seq ~this:(Some (VObj obj)) method_.code local_env);
      Null
    

    and exec_seq ?(this=None) seq env =
      try List.iter (fun instr -> exec_instr ~this instr env) seq
      with Return v -> ()
      

    and exec_instr ?(this=None) i env =
        match i with
    | Print e ->
        (match eval_expr e env with
         | VInt n -> Printf.printf "%d\n" n
         | _ -> error "Print expects an integer")
    | Set (Var x, e) ->
        let v = eval_expr e env in
        Hashtbl.replace env x v
    | Set (Field (obj_expr, field), e) ->
        let v = eval_expr e env in
        (match eval_expr obj_expr env with
         | VObj obj -> Hashtbl.replace obj.fields field v
         | _ -> error "Field assignment on non-object")
    | If (cond, then_seq, else_seq) ->
        (match eval_expr cond env with
         | VBool true -> exec_seq then_seq env 
         | VBool false -> exec_seq else_seq env 
         | _ -> error "If condition must be a boolean")
    | While (cond, body) ->
        let rec loop () =
          match eval_expr cond env with
          | VBool true -> exec_seq body env ; loop ()
          | VBool false -> ()
          | _ -> error "While condition must be a boolean"
        in
        loop ()
    | Return e -> raise (Return (eval_expr e env))
    | Expr e -> ignore (eval_expr e env)

  and find_class cname classes =
    match List.find_opt (fun c -> c.class_name = cname) classes with
    | Some cls -> cls
    | None -> error ("Class not found: " ^ cname)

  and add_params_to_env params args env =
    let local_env = Hashtbl.copy env in
    List.iter2 (fun (name, _) arg -> Hashtbl.add local_env name arg) params args;
    local_env

  in
  ignore (exec_seq p.main env )
