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
        List.iter (fun (field, _) -> Hashtbl.add fields field Null) cls.attributes;
        VObj { cls = cname; fields }
    | None -> error ("Class not found: " ^ cname)
  in
  let rec eval_expr e env this =
    match e with
    | Int n -> VInt n
    | Bool b -> VBool b
    | Unop (Opp, e1) ->
        (match eval_expr e1 env this with
         | VInt n -> VInt (-n)
         | _ -> error "Unary '-' applied to non-integer")
    | Unop (Not, e1) ->
        (match eval_expr e1 env this with
         | VBool b -> VBool (not b)
         | _ -> error "Unary 'not' applied to non-boolean")
    | Binop (op, e1, e2) ->
        let v1 = eval_expr e1 env this in
        let v2 = eval_expr e2 env this in
        eval_binop op v1 v2
    | Get (Var x) ->
        (try Hashtbl.find env x
         with Not_found -> error ("Variable not found: " ^ x))
    | Get (Field (e, field)) ->
        (match eval_expr e env this with
         | VObj o -> (try Hashtbl.find o.fields field
                      with Not_found -> error ("Field not found: " ^ field))
         | _ -> error "Field access on non-object")
    | This -> (match this with
               | Some obj -> obj
               | None -> error "Unbound 'this' in the current context")
    | New cname -> create_object cname
    | NewCstr (cname, args) ->
        let obj = create_object cname in
        let cls = find_class cname p.classes in
        let constructor =
          match List.find_opt (fun m -> m.method_name = "constructor") cls.methods with
          | Some cstr -> cstr
          | None -> error ("Constructor not found for class: " ^ cname)
        in
        let arg_values = List.map (fun a -> eval_expr a env this) args in
        let local_env = add_params_to_env constructor.params arg_values env in
        ignore (exec_seq constructor.code local_env (Some (obj)));
        obj
    | MethCall (obj_expr, mname, args) ->
        (match eval_expr obj_expr env this with
         | VObj obj -> call_method obj mname args env this
         | _ -> error "Method call on non-object")

  and eval_binop op v1 v2 =
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

  and call_method obj mname args env this =
    let cls = find_class obj.cls p.classes in
    let method_ =
      match List.find_opt (fun m -> m.method_name = mname) cls.methods with
      | Some m -> m
      | None -> error ("Method not found: " ^ mname)
    in
    let arg_values = List.map (fun a -> eval_expr a env this) args in
    let local_env = add_params_to_env method_.params arg_values env in
    try
      exec_seq method_.code local_env (Some (VObj obj));
      Null
    with Return v -> v

  and exec_seq seq env this =
    try List.iter (fun instr -> exec_instr instr env this) seq
    with Return v -> ()

  and exec_instr i env this =
    match i with
    | Print e ->
        (match eval_expr e env this with
         | VInt n -> Printf.printf "%d\n" n
         | _ -> error "Print expects an integer")
    | Set (Var x, e) ->
        let v = eval_expr e env this in
        Hashtbl.replace env x v
    | Set (Field (obj_expr, field), e) ->
        let v = eval_expr e env this in
        (match eval_expr obj_expr env this with
         | VObj obj -> Hashtbl.replace obj.fields field v
         | _ -> error "Field assignment on non-object")
    | If (cond, then_seq, else_seq) ->
        (match eval_expr cond env this with
         | VBool true -> exec_seq then_seq env this
         | VBool false -> exec_seq else_seq env this
         | _ -> error "If condition must be a boolean")
    | While (cond, body) ->
        let rec loop () =
          match eval_expr cond env this with
          | VBool true -> exec_seq body env this; loop ()
          | VBool false -> ()
          | _ -> error "While condition must be a boolean"
        in
        loop ()
    | Return e -> raise (Return (eval_expr e env this))
    | Expr e -> ignore (eval_expr e env this)

  and find_class cname classes =
    match List.find_opt (fun c -> c.class_name = cname) classes with
    | Some cls -> cls
    | None -> error ("Class not found: " ^ cname)

  and add_params_to_env params args env =
    let local_env = Hashtbl.copy env in
    List.iter2 (fun (name, _) arg -> Hashtbl.add local_env name arg) params args;
    local_env

  in
  ignore (exec_seq p.main env None)
