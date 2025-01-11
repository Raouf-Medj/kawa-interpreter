open Kawa

type value =
  | VInt of int
  | VBool of bool
  | VObj of obj
  | VArray of value array
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
  (* Table globale pour stocker les attributs statiques des classes *)
let static_fields = Hashtbl.create 16
in 
(* Initialiser les attributs statiques d'une classe *)
let init_static_fields cls =
  match Hashtbl.find_opt static_fields cls.class_name with
  | None -> 
      (* Initialise les valeurs des champs statiques *)
      let fields = Hashtbl.create 16 in
      cls.static_attribut 
      |> List.iter (fun (field, is_static) -> 
          if is_static then Hashtbl.add fields field Null);
      Hashtbl.add static_fields cls.class_name fields
  | Some _ -> () (* Ne rien faire si déjà initialisé *)

in
(* Fonction de création d'un objet d'instance *)
let create_object cname =
  match List.find_opt (fun c -> c.class_name = cname) p.classes with
  | Some cls ->
      (* Initialisation des attributs statiques si ce n'est pas encore fait *)
      (**********************)
      init_static_fields cls;
      (* Création des attributs d'instance *)
      let fields = Hashtbl.create 16 in
      List.iter 
        (fun (field, _) -> 
          if not (List.exists (fun (sfield, test) -> (sfield = field) && (test)) cls.static_attribut)
          then begin Hashtbl.add fields field Null end
        )
        (collect_attributes cls);

      VObj { cls = cname; fields = fields }
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
    | EArrayCreate (typ, dims) -> 
      (match dims with
      | [] -> error "Array dimensions cannot be empty"
      | _ ->
          (* Évalue les dimensions *)
          let dim_sizes = List.map (fun dim_expr ->
            match eval_expr dim_expr env this super with
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
          match eval_expr idx_expr env this super with
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

  and class_incluse classes cname1 cname2 =
    if cname1 = cname2 then true
    else
      let cls = find_class cname1 classes in
      match cls.parent with
      | Some parent -> class_incluse classes parent cname2
      | None -> false
         | Get (Field (e, field)) ->
          (match eval_expr e env this with
           | VObj o ->
               (try 
                  (* Recherche dans les champs d'instance *)
                  Hashtbl.find o.fields field
                with Not_found -> 
                  (* Si introuvable, rechercher dans les champs statiques *)
                  let cname = o.cls in
                  (match Hashtbl.find_opt static_fields cname with
                   | Some class_static_fields ->
                       (try 
                          Hashtbl.find class_static_fields field
                        with Not_found -> 
                          error ("Field not found: " ^ field))
                   | None ->
                       error ("Static fields not initialized for class: " ^ cname)))
           | _ -> error "Field access on non-object")

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
    | _, _ -> false

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
    | Set (Var x, e) ->
        let v = eval_expr e env this super in
        Hashtbl.replace env x v
    | Set (Field (obj_expr, field), e) ->
      let v = eval_expr e env this in
      (match eval_expr obj_expr env this with
      | VObj obj -> 
          let cls = find_class obj.cls p.classes in
          let is_final = find_field_is_final cls field in
        
          
          if not (List.exists (fun (sfield, test) -> (sfield = field) && (test)) cls.static_attribut) then
          let vfield = 
            (try Hashtbl.find obj.fields field
              with Not_found -> try Hashtbl.find env field
                                with Not_found -> (*Hashtbl.iter (fun x y -> Printf.printf "Hello %s" x ) obj.fields;*) error ("Hna Field not found: " ^ field))
          in
          if is_final && vfield <> Null then error ("Field is final: " ^ field);
            (* Mise à jour du champ d'instance *)
            Hashtbl.replace obj.fields field v 
          else
            (* Mise à jour du champ statique *)
            let cname = obj.cls in
            (match Hashtbl.find_opt static_fields cname with
            | Some class_static_fields ->
                (* Champ statique déjà initialisé pour cette classe *)
                Hashtbl.replace class_static_fields field v
            | None ->
                (* Impossible, champs statiques non initialisés *)
                error ("Static fields not initialized for class: " ^ cname)
            )
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
