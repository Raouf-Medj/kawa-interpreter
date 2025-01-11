
open Kawa

type  value =
  | VInt of int
  | VBool of bool
  | VObj of obj
  | VArray of value array  (* Tableau statique *)
  | Null

and obj = {
  cls: string;
  fields: (string, value) Hashtbl.t;
}

let typ_to_string = function 
| VInt _->"int"
| VBool _ ->"bool"
| VObj _ -> "obj"
| VArray v -> "array"
| Null -> "Null"

let rec init_value = function 
TInt -> (VInt 0)
| TBool -> (VBool false)
| TVoid -> Null
| TClass _ ->  Null
|TArray t -> VArray (Array.make 0 (init_value t)) 

let extract_option = function
  | Some value -> value  (* Retourne la valeur contenue dans l'option *)
  | None -> Null  (* Lève une exception si l'option est vide *)


let rec create_array dims t =
  match dims with
  | [] -> failwith "Dimensions list cannot be empty"
  | [dim] -> (match t with 
            TInt -> VArray (Array.make dim (VInt 0))
            | TBool -> VArray (Array.make dim (VBool false))
            | TVoid -> VArray (Array.make dim Null)
            | TClass _ -> VArray (Array.make dim Null)
            | TArray t -> let core_type = Typechecker.get_array_core_type t in VArray (Array.make dim (init_value core_type))   
          )
  | dim :: rest ->VArray (Array.make dim (create_array rest t))
exception Error of string
exception Return of value

let error s = raise (Error s)


let rec exec_prog (p: program): unit =

  let create_object cname =
    match List.find_opt (fun c -> c.class_name = cname) p.classes with
    | Some cls ->
        let fields = Hashtbl.create 16 in
        List.iter (fun (field, _, _) -> Hashtbl.add fields field Null) cls.attributes;
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
      
    (*| This -> (match this with
               | Some obj -> obj
               | None -> error "Unbound 'this' in the current context")
    *)
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
    
    | EArrayCreate (typ, dims) -> 
        (match dims with
        | [] -> error "Array dimensions cannot be empty"
        | _ ->
            (* Évalue les dimensions *)
            let dim_sizes = List.map (fun dim_expr ->
              match eval_expr dim_expr env this with
              | VInt size when size > 0 -> size
              | VInt size when size <= 0 -> error "Array size must be positive"
              | _ -> error "Array dimensions must be integers"
            ) dims in
            
            (* Fonction pour obtenir la valeur par défaut pour le type *)
            let default_value_for_type typ =
              match typ with
              | TInt -> VInt 0
              | TBool -> VBool false
              | TVoid -> Null
              | TClass _ -> Null
              | TArray inner_type -> 
                  (* Si c'est un tableau, on retourne une valeur par défaut du type de tableau intérieur *)
                  VArray (Array.make 0 Null)  (* Tableau vide pour des tableaux imbriqués *)
              (*| _ -> error "Unsupported array element type"*)
            in
            
            (* Fonction récursive pour créer un tableau multidimensionnel *)
            let rec create_nested_array sizes =
              match sizes with
              | [last_dim] -> VArray (Array.make last_dim (default_value_for_type typ))  (* Dernier niveau du tableau *)
              | dim :: rest ->
                  (* Crée une dimension contenant des sous-tableaux *)
                  let inner_array = create_nested_array rest in
                  VArray (Array.init dim (fun _ -> inner_array))
              | [] -> error "Unexpected empty dimension list during array creation"
            in

            (* Crée le tableau avec les tailles données *)
            let array_value = create_nested_array dim_sizes in

            (* Retourner la valeur du tableau créé *)
            array_value)
 
    | Get (ArrayAccess (array_name, indices)) ->
        (try
          let array_val = Hashtbl.find env array_name in
          let indices_values = List.map (fun idx_expr ->
            match eval_expr idx_expr env this with
            | VInt idx when idx >= 0 -> idx
            | VInt idx when idx < 0 -> error "Array index must be non-negative"
            | _ -> error "Array indices must be integers"
          ) indices in
          let rec access_nested_array arr dims =
            match arr, dims with
            | VArray nested_array, idx :: rest when idx < Array.length nested_array ->
                if rest = [] then nested_array.(idx)
                else access_nested_array nested_array.(idx) rest
            | VArray _, idx :: _ -> error "Array index out of bounds"
            | _, _ -> error "Invalid array access"
          in
          access_nested_array array_val indices_values
        with Not_found -> error ("Array not found: " ^ array_name))
| InstanceOf (expression, cname) ->
    (* Évaluer l'expression pour obtenir une valeur *)
    let value = eval_expr expression env this in
    (match value with
     | VObj obj ->
         (* Vérifier si la classe de l'objet est compatible avec le type demandé *)
         let rec is_subclass child parent =
           if child = parent then true
           else
             match List.find_opt (fun c -> c.class_name = child) p.classes with
             | Some cls -> 
                 (match cls.parent with
                  | Some super -> is_subclass super parent
                  | None -> false)
             | None -> error ("Class not found: " ^ child)
         in
         VBool (is_subclass obj.cls cname)
     | Null -> VBool false
     | _ -> error ("INSTANCEOF applied to a non-object value"))

  
    

    
    
       

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
    | Structeg, v1, v2 -> VBool (structural_eq v1 v2)
    | Structineg, v1, v2 -> VBool (not (structural_eq v1 v2))
    | _ -> error "Invalid binary operation or operand types"

  (* Fonction pour l'égalité structurelle *)
  and structural_eq v1 v2 =
    match v1, v2 with
    | VInt n1, VInt n2 -> n1 = n2
    | VBool b1, VBool b2 -> b1 = b2
    | VObj o1, VObj o2 ->
        o1.cls = o2.cls &&
        (* Comparer les champs des deux objets *)
        Hashtbl.fold (fun field_name field_value acc ->
          acc && 
          (try structural_eq field_value (Hashtbl.find o2.fields field_name)
           with Not_found -> false)
        ) o1.fields true
    | VArray arr1, VArray arr2 ->
        Array.length arr1 = Array.length arr2 &&
        Array.for_all2 structural_eq arr1 arr2
    | Null, Null -> true
    | _, _ -> false  (* Types différents ou valeurs non compatibles *)


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
    with Return v -> raise (Return v)

  and exec_instr i env this =
    match i with
    | Print e ->
      (match eval_expr e env this with
       | VInt n -> Printf.printf "%d\n" n
       | VBool b -> Printf.printf "%b\n" b
       | VObj _ -> Printf.printf "<object>\n"
       | VArray arr ->
           (* Affiche les éléments du tableau statique *)
           Printf.printf "[";
           Array.iteri (fun i value ->
             (match value with
              | VInt n -> Printf.printf "%d" n
              | VBool b -> Printf.printf "%b" b
              | VObj _ -> Printf.printf "<object>"
              | VArray _ -> Printf.printf "<nested array>"
              | Null -> Printf.printf "null");
             if i < Array.length arr - 1 then Printf.printf ", ";
           ) arr;
           Printf.printf "]\n";
       | Null -> Printf.printf "null\n")
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
    | Set (m, e) ->
      match m with
      | Var name ->
          let v = eval_expr e env this in
          Hashtbl.replace env name v
      | Field (obj_expr, field_name) ->
          (match eval_expr obj_expr env this with
          | VObj obj ->
              let v = eval_expr e env this in
              Hashtbl.replace obj.fields field_name v
          | _ -> error "Field assignment on non-object")
      | ArrayAccess (array_name, indices) ->
            (* Evaluate all index expressions *)
            let index_vals = List.map (fun index_expr -> 
                match eval_expr index_expr env this with
                | VInt idx -> idx
                | _ -> error "Array indices must be integers"
            ) indices in
            
            (* Retrieve the array value from the environment *)
            (match Hashtbl.find_opt env array_name with
            | Some (VArray arr) ->
                let rec set_value_in_array nested_array index_list new_value =
                  match index_list with
                  | [last_idx] -> 
                      if last_idx >= 0 && last_idx < Array.length nested_array then
                        nested_array.(last_idx) <- new_value
                      else error "Array index out of bounds"
                  | idx :: rest ->
                      if idx >= 0 && idx < Array.length nested_array then
                        (match nested_array.(idx) with
                         | VArray sub_array -> set_value_in_array sub_array rest new_value
                         | _ -> error "Invalid array structure during access")
                      else error "Array index out of bounds"
                  | [] -> error "Index list cannot be empty"
                in
                
                (* Evaluate the new value and assign it to the array *)
                let new_value = eval_expr e env this in
                set_value_in_array arr index_vals new_value
            | _ -> error "Array not found or invalid structure for assignment")
        

  and find_class cname classes =
    match List.find_opt (fun c -> c.class_name = cname) classes with
    | Some cls -> cls
    | None -> error ("Class not found: " ^ cname)

    (*and add_params_to_env params args env =
      let local_env = Hashtbl.copy env in
      List.iter2 
        (fun (name, typ, init_opt) arg -> 
           (* Si la valeur initiale est définie, on utilise `init_opt`, sinon on utilise `arg` *)
           let value = match init_opt with
             | Some init -> (eval_expr init env this)
             | None -> arg
           in
           Hashtbl.add local_env name value
        ) 
        params args;
      local_env*)
   and add_params_to_env params args env =
        let local_env = Hashtbl.copy env in
        List.iter2 (fun (name, _, _) arg -> Hashtbl.add local_env name arg) params args;
        local_env
    
  in
  let env = Hashtbl.create 16 in
  (* Initialize global variables *)
  List.iter (fun (x, _, init) ->
    match init with 
    | Some v -> Hashtbl.add env x (eval_expr v env None)
    |None-> Hashtbl.add env x Null 
  )p.globals;
  ignore (exec_seq p.main env None)
