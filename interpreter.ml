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

  let rec collect_attributes cls =
    let parent_attributes = match cls.parent with
      | Some parent_name ->
          let parent_cls = 
            (match List.find_opt (fun c -> c.class_name = parent_name) p.classes with
            | Some cls -> cls
            | None -> error ("Class not found: " ^ cls.class_name)) in
          collect_attributes parent_cls
      | None -> []
    in
    parent_attributes @ cls.attributes
  in
  let create_object cname =
    match List.find_opt (fun c -> c.class_name = cname) p.classes with
    | Some cls ->
        let fields = Hashtbl.create 16 in
        List.iter (fun (field, _) -> Hashtbl.add fields field Null) (collect_attributes cls);
        VObj { cls = cname; fields }
    | None -> error ("Class not found: " ^ cname)
  in
  let rec eval_expr e env this super =
    match e with
    | Int n -> VInt n
    | Bool b -> VBool b
    | Unop (Opp, e1) ->
        (match eval_expr e1 env this super with
         | VInt n -> VInt (-n)
         | _ -> error "Unary '-' applied to non-integer")
    | Unop (Not, e1) ->
        (match eval_expr e1 env this super with
         | VBool b -> VBool (not b)
         | _ -> error "Unary 'not' applied to non-boolean")
    | Binop (op, e1, e2) ->
        let v1 = eval_expr e1 env this super in
        let v2 = eval_expr e2 env this super in
        eval_binop op v1 v2
    | Get (Var x) ->
        (try Hashtbl.find env x
         with Not_found -> error ("Variable not found: " ^ x))
    | Get (Field (e, field)) ->
        (match eval_expr e env this super with
         | VObj o -> (try Hashtbl.find o.fields field
                      with Not_found -> error ("Field not found: " ^ field))
         | _ -> error "Field access on non-object")
    | This -> (match this with
               | Some obj -> obj
               | None -> error "Unbound 'this' in the current context")
    | Super -> (match super with
                | Some obj -> obj
                | None -> error "Unbound 'super' in the current context")
    | New cname -> create_object cname
    | NewCstr (cname, args) ->
        let obj = create_object cname in
        let cls = find_class cname p.classes in
        let constructor =
          match List.find_opt (fun m -> m.method_name = "constructor") cls.methods with
          | Some cstr -> cstr
          | None -> error ("Constructor not found for class: " ^ cname)
        in
        let arg_values = List.map (fun a -> eval_expr a env this super) args in
        let local_env = add_params_to_env constructor.params arg_values env in
        exec_seq constructor.code local_env (Some (obj)) super;
        obj
    | MethCall (obj_expr, mname, args) ->
        (match eval_expr obj_expr env this super with
         | VObj obj -> call_method obj mname args env this super
         | _ -> error "Method call on non-object")
    | InstanceOf (e, cname) -> 
        (match eval_expr e env this super with
         | VObj obj -> VBool (class_incluse p.classes obj.cls cname)
         | _ -> error "Instanceof on non-object")

  and class_incluse classes cname1 cname2 =
    if cname1 = cname2 then true
    else
      let cls = find_class cname1 classes in
      match cls.parent with
      | Some parent -> class_incluse classes parent cname2
      | None -> false

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

  and call_method obj mname args env this super =
    let cls = find_class obj.cls p.classes in
    let method_ =
      match List.find_opt (fun m -> m.method_name = mname) cls.methods with
      | Some m -> m
      | None -> error ("Method not found: " ^ mname)
    in
    let arg_values = List.map (fun a -> eval_expr a env this super) args in
    let local_env = add_params_to_env method_.params arg_values env in
    try
      let parent = match cls.parent with Some parent -> parent | None -> "void" in
      exec_seq method_.code local_env (Some (VObj obj)) (Some (VObj { cls = parent; fields = obj.fields }));
      Null
    with Return v -> v

  and exec_seq seq env this super =
    try List.iter (fun instr -> exec_instr instr env this super) seq
    with Return v -> raise (Return v)
  
  and find_field_is_final cls field =
    try List.assoc field cls.is_attr_final
    with Not_found ->
      match cls.parent with
      | Some parent_name ->
          let parent_cls = find_class parent_name p.classes in
          find_field_is_final parent_cls field
      | None -> error ("Field not found: " ^ field)
  
  and exec_instr i env this super =
    match i with
    | Print e ->
        (match eval_expr e env this super with
         | VInt n -> Printf.printf "%d\n" n
         | VBool b -> Printf.printf "%b\n" b
         | VObj _ -> Printf.printf "<object>\n"
         | Null -> Printf.printf "null\n")
    | Set (Var x, e) ->
        let v = eval_expr e env this super in
        Hashtbl.replace env x v
    | Set (Field (obj_expr, field), e) ->
        let v = eval_expr e env this super in
        (match eval_expr obj_expr env this super with
         | VObj obj -> 
        let is_final = (let cls = find_class obj.cls p.classes in find_field_is_final cls field) in
        let vfield = (try Hashtbl.find obj.fields field with Not_found -> try Hashtbl.find env field with Not_found -> error ("Field not found: " ^ field)) in
        if (is_final && vfield <> Null) then error ("Field is final: " ^ field);
          Hashtbl.replace obj.fields field v
         | _ -> error "Field assignment on non-object")
    | If (cond, then_seq, else_seq) ->
        (match eval_expr cond env this super with
         | VBool true -> exec_seq then_seq env this super
         | VBool false -> exec_seq else_seq env this super
         | _ -> error "If condition must be a boolean")
    | While (cond, body) ->
        let rec loop () =
          match eval_expr cond env this super with
          | VBool true -> exec_seq body env this super; loop ()
          | VBool false -> ()
          | _ -> error "While condition must be a boolean"
        in
        loop ()
    | Return e -> raise (Return (eval_expr e env this super))
    | Expr e -> ignore (eval_expr e env this super)

  and find_class cname classes =
    match List.find_opt (fun c -> c.class_name = cname) classes with
    | Some cls -> cls
    | None -> error ("Class not found: " ^ cname)

  and add_params_to_env params args env =
    let local_env = Hashtbl.copy env in
    List.iter2 (fun (name, _) arg -> Hashtbl.add local_env name arg) params args;
    local_env

  in
  exec_seq p.main env None None
