open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))


           (* Fonction pour calculer la distance de Levenshtein *)
let levenshtein_distance s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let dp = Array.make_matrix (len1 + 1) (len2 + 1) 0 in

  (* Initialisation des bords *)
  for i = 0 to len1 do
    dp.(i).(0) <- i
  done;
  for j = 0 to len2 do
    dp.(0).(j) <- j
  done;

  (* Calcul de la distance *)
  for i = 1 to len1 do
    for j = 1 to len2 do
      let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
      dp.(i).(j) <- min (min 
                          (dp.(i - 1).(j) + 1)  (* Suppression *)
                          (dp.(i).(j - 1) + 1)) (* Insertion *)
                          (dp.(i - 1).(j - 1) + cost); (* Substitution *)
    done;
  done;

  dp.(len1).(len2)

(* Fonction principale pour trouver le mot le plus proche *)
let closest_match target identifiers =
  match identifiers with
  | [] -> " "
  | _ ->
    let closest =
      List.fold_left
        (fun (best_name, best_distance) current ->
           let distance = levenshtein_distance target current in
           if distance < best_distance then (current, distance) else (best_name, best_distance))
        ("", max_int) identifiers
    in
    match closest with
    | (best_name, _) when best_name <> "" -> best_name
    | _ -> " "

(*let closest_lexicographically target identifiers =
  match identifiers with
  | [] -> failwith "The list of identifiers is empty"
  | _ ->
      List.fold_left (fun closest current ->
        match closest with
        | None -> Some current
        | Some best ->
            if abs (String.compare target current) < abs (String.compare target best)
            then Some current
            else closest
      ) None identifiers
      |> function
      | Some result -> result
      | None -> failwith " "*)

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l
let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec find_class classes cname =
    try List.find (fun c -> c.class_name = cname) classes
    with Not_found -> error ("Class not found: " ^ cname ^ 
                            ", did you mean: " ^ 
                            (closest_match cname (List.map (fun c -> c.class_name) classes)) ^ "?" )

  and find_method cls mname =
    try List.find (fun m -> m.method_name = mname) cls.methods
    with Not_found -> error ("Method not found: " ^ mname ^
                            ", did you mean: " ^ 
                            (closest_match mname (List.map (fun c -> c.method_name) cls.methods)) ^ "?" )

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

  and reduce_dim t dims =
    match dims with
    | [] -> t
    | hd :: tl ->
        check hd TInt tenv;
        (match t with
          | TArray elem_type -> reduce_dim elem_type tl
          | _ -> error "Dimension mismatch: expected an array")

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
          check e1 TBool tenv; check e2 TBool tenv; TBool
      | Structeg | Structineg -> 
          let t1 = type_expr e1 tenv in
          let t2 = type_expr e2 tenv in
          if t1 <> t2 then type_error t2 t1;
          TBool)
    (*| Get (Var x) -> (try Env.find x tenv with Not_found -> error ("Undeclared variable: " ^ x))
    | Get (Field (e, field)) ->
      (match type_expr e tenv with
      | TClass cname ->
          let cls = find_class p.classes cname in
          find_field_type cls field
      | ty -> type_error ty (TClass "object"))
    | Get (ArrayAccess(name, indices)) -> (
      try
        let arr_type = Env.find name tenv in
        try reduce_dim arr_type indices
        with Error msg -> error msg
      with Not_found ->
        error ("Undeclared variable: "))*)
        | Get (Var x) -> 
          (try Env.find x tenv 
           with Not_found -> 
             let suggestions = 
               closest_match x (List.map fst (Env.bindings tenv)) 
             in
             error ("Undeclared variable: " ^ x ^ 
                    ", did you mean: " ^ suggestions ^ "?"))
      | Get (Field (e, field)) -> 
          (match type_expr e tenv with
           | TClass cname -> 
               let cls = find_class p.classes cname in
               (try find_field_type cls field 
                with Error _ ->
                  let suggestions = 
                    closest_match field 
                    (List.map fst cls.attributes)
                  in
                  error ("Field not found: " ^ field ^ 
                         ", did you mean: " ^ suggestions ^ "?"))
           | ty -> type_error ty (TClass "object"))
      | Get (ArrayAccess (name, indices)) -> 
          (try 
             let arr_type = Env.find name tenv in
             try reduce_dim arr_type indices 
             with Error msg -> error msg
           with Not_found -> 
             let suggestions = 
               closest_match name (List.map fst (Env.bindings tenv)) 
             in
             error ("Undeclared variable: " ^ name ^ 
                    ", did you mean: " ^ suggestions ^ "?"))
  
    | This -> 
      (try Env.find "this" tenv with Not_found -> error ("Unbound 'this' in the current context"))
    | Super -> 
      (try Env.find "super" tenv with Not_found -> error ("Unbound 'super' in the current context"))
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
            if cname = "void" then error "Class has no superclass";
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
    | EArrayCreate(t, n) ->
      List.iter (fun x -> check x TInt tenv) n;
      let rec retu n = 
        if n == 1 then t
        else TArray (retu (n-1)) 
      in  TArray (retu (List.length n))

  
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
    | Set (ArrayAccess (arr_expr, index_expr), value_expr) -> 
      let tvar =
        try
          let arr_type = Env.find arr_expr tenv in
          try reduce_dim arr_type index_expr
          with Error msg -> error msg
        with Not_found ->
          error ("Undeclared variable: ")
      in check value_expr tvar tenv
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
    let class_env_this_super = match cls.parent with 
    | Some parent -> Env.add "super" (TClass(parent)) class_env_this 
    | None -> Env.add "super" (TClass("void")) class_env_this 
    in
    List.iter (fun method_ -> check_method cls method_ class_env_this_super) cls.methods

  in
  List.iter (fun cls -> check_class cls tenv) p.classes;
  check_seq p.main TVoid tenv "main"
