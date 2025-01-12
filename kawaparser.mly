%{

  open Lexing
  open Kawa

  let rec tarray_maker d t= if(d=1) then TArray t else TArray (tarray_maker (d-1) t)  
%}


%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA DOT SET RBRACKET(* [ *) LBRACKET(* ] *)
%token VAR ATTR METHOD CLASS NEW THIS EXTENDS
%token TINT TBOOL TVOID
%token TRUE FALSE
%token IF ELSE WHILE RETURN
%token ADD DIV SUB MUL REM OR AND NOT
%token EQ NEQ LT LE GT GE STRUCTEG STRUCTINEG
%token PRINT
%token EOF
%token FINAL INSTANCEOF SUPER STATIC


// %right SET
%left OR
%left AND
%left EQ NEQ
%left STRUCTEG STRUCTINEG
%left LT LE GT GE INSTANCEOF
%left ADD SUB
%left MUL DIV REM
%right NEG
%right NOT
%left DOT 


%start program
%type <Kawa.program> program

%%

program:
| vrs=list(var_decl) cls=list(class_def) MAIN BEGIN main=list(instr) END EOF
    { {
      classes=cls;
      globals=
        (let glb = List.fold_left (fun acc l -> acc @ l) [] vrs in
        let has_duplicates lst =
          let tbl = Hashtbl.create (List.length lst) in
          List.fold_left (fun found x ->
            if found then true
            else if Hashtbl.mem tbl x then true
            else (Hashtbl.add tbl x (); false)
          ) false lst in
        if has_duplicates (List.map (fun (id, _, _) -> id) glb) then failwith "Duplicate variable declaration"
        else List.map (fun (id, ty, _) -> (id, ty)) glb); 
      globals_init_vals = 
        (let glb = List.fold_left (fun acc l -> acc @ l) [] vrs in
        List.map (fun (id, _, init) -> (id, init)) glb);
      main
    } }
;

class_def:
| CLASS IDENT opt_parent BEGIN list(attr_decl) list(method_def) END {
    {
      class_name = $2; (* Nom de la classe *)
      parent = $3; (* Classe parent, si elle existe *)
      attributes = List.map (fun (id, typ, _, _, _) -> (id, typ)) $5; (* Liste des attributs *)
      methods = $6; (* Liste des méthodes *)
      is_attr_final = List.map (fun (id, _, is_final, _, _) -> (id, is_final)) $5; (* Finalité des attributs *)
      static_attribut = List.map (fun (id, _, _, is_static, _) -> (id, is_static)) $5; (* Attributs statiques *)
      attr_init_vals = List.map (fun (id, _, _, _, init) -> (id, init)) $5; (* Valeurs initiales des attributs *)
    }
  }
;

var_decl:
| VAR typpc separated_nonempty_list(COMMA, ident_init) SEMI { List.map (fun (ident, init) -> (ident, $2, init)) $3 }
;

attr_decl:
| ATTR typpc IDENT init SEMI { ($3, $2, false, false, $4) }
| ATTR FINAL typpc IDENT init SEMI { ($4, $3, true, false, $5) }
| ATTR STATIC typpc IDENT init SEMI { ($4, $3, false, true, $5) }
| ATTR STATIC FINAL typpc IDENT SET expr SEMI | ATTR FINAL STATIC typp IDENT SET expr SEMI { ($5, $4, true, true, Some($7)) }
;

init:
| SET expr { Some($2) }
| (* empty *) { None }
;

ident_init:
| IDENT { ($1, None) }  (* IDENT without initialization *)
| IDENT SET expr { ($1, Some($3)) }  (* IDENT with initialization *)
;

param_decl:
| typpc IDENT { ($2, $1) }
;

method_def:
| METHOD tp=typpc id=IDENT LPAR param_lst=separated_list(COMMA, param_decl) RPAR BEGIN locs=list(var_decl) sequence=list(instr) END { {
    method_name = id;
    code = sequence;
    params = param_lst;
    locals = 
      (let loc = List.fold_left (fun acc l -> acc @ l) [] locs in
      let has_duplicates lst =
        let tbl = Hashtbl.create (List.length lst) in
        List.fold_left (fun found x ->
          if found then true
          else if Hashtbl.mem tbl x then true
          else (Hashtbl.add tbl x (); false)
        ) false lst in
      if has_duplicates (List.map (fun (id, _, _) -> id) loc) then failwith "Duplicate variable declaration"
        else List.map (fun (id, ty, _) -> (id, ty)) loc); 
    locals_init_vals =
      (let loc = List.fold_left (fun acc l -> acc @ l) [] locs in
      List.map (fun (id, _, init) -> (id, init)) loc);
    return = tp;
  } }
;

opt_parent:
| EXTENDS IDENT { Some($2) }
| (* empty *) { None }
;

typp:
| TINT { TInt }
| TBOOL { TBool }
| TVOID { TVoid }
| IDENT { TClass($1) }
;

typpc: 
| t= typp {t}
| t= typp d=nonempty_list(bracket_pair) { tarray_maker (List.length d) t}
;

%inline bracket_pair :
| LBRACKET RBRACKET { 0 }

instr:
| PRINT LPAR e=expr RPAR SEMI { Print(e) }
| mem SET expr SEMI { Set($1, $3) }
| IF LPAR e=expr RPAR BEGIN b1=list(instr) END ELSE BEGIN b2=list(instr) END { If(e, b1, b2) }
| IF LPAR e=expr RPAR BEGIN b1=list(instr) END  { UIf(e, b1) }
| WHILE LPAR e=expr RPAR BEGIN b=list(instr) END { While(e, b) }
| RETURN expr SEMI { Return($2) }
| expr SEMI { Expr($1) }
;

expr:
| INT { { annot = TInt; expr = Int($1); loc = fst $loc } }
| TRUE { { annot = TBool; expr = Bool(true); loc = fst $loc } }
| FALSE { { annot = TBool; expr = Bool(false); loc = fst $loc} }
| THIS { { annot = TClass("this"); expr = This; loc = fst $loc } }
| SUPER { { annot = TClass("super"); expr = Super; loc = fst $loc} }
| mem {
    let annotation = match $1 with
        | Var _ -> TVoid (* A changer *)
        | Field (_, _) -> TClass("Unknown") (* Adjust based on the correct type for a field *)
        | ArrayAccess (_, _) -> TArray(TInt) (* Adjust based on the correct type for an array access *)
    in
    { annot = annotation; expr = Get($1); loc = fst $loc }
}
| expr SUB expr { {annot = TInt; expr = Binop(Sub, $1, $3); loc = fst $loc  } }
| expr ADD expr { {annot = TInt; expr = Binop(Add, $1, $3); loc = fst $loc  } }
| expr MUL expr { {annot = TInt; expr = Binop(Mul, $1, $3); loc = fst $loc  } }
| expr DIV expr { {annot = TInt; expr = Binop(Div, $1, $3); loc = fst $loc } }
| expr REM expr { {annot = TInt; expr = Binop(Rem, $1, $3); loc = fst $loc } }
| expr LT expr { {annot = TBool; expr = Binop(Lt, $1, $3); loc = fst $loc } }
| expr LE expr { { annot = TBool; expr = Binop(Le, $1, $3); loc = fst $loc } }
| expr GT expr { { annot = TBool; expr = Binop(Gt, $1, $3); loc = fst $loc } }
| expr GE expr { { annot = TBool; expr = Binop(Ge, $1, $3); loc = fst $loc } }
| expr EQ expr { { annot = TBool; expr = Binop(Eq, $1, $3); loc = fst $loc } }
| expr NEQ expr { { annot = TBool; expr = Binop(Neq, $1, $3); loc = fst $loc } }
| expr AND expr { { annot = TBool; expr = Binop(And, $1, $3); loc = fst $loc } }
| expr OR expr { { annot = TBool; expr = Binop(Or, $1, $3); loc = fst $loc } }
| expr STRUCTEG expr { { annot = TBool; expr = Binop(Structeg, $1, $3); loc = fst $loc } }
| expr STRUCTINEG expr { { annot = TBool; expr = Binop(Structineg, $1, $3); loc = fst $loc } }
| SUB expr %prec NEG { { annot = TInt; expr = Unop(Opp, $2); loc = fst $loc } }
| NOT expr { { annot = TBool; expr = Unop(Not, $2); loc = fst $loc } }
| LPAR expr RPAR { $2 } 
| NEW IDENT { { annot = TClass($2); expr = New($2); loc = fst $loc } }
| NEW IDENT LPAR separated_list(COMMA, expr) RPAR { 
    { 
      annot = TClass($2); 
      expr = NewCstr($2, $4);
      loc = fst $loc
    } 
  }
| expr DOT IDENT LPAR separated_list(COMMA, expr) RPAR 
    { { annot = TVoid; expr = MethCall($1, $3, $5); loc = fst $loc } }
| NEW typp nonempty_list(list_array) 
    { { annot = TArray($2); expr = EArrayCreate($2, $3); loc = fst $loc } }
| expr INSTANCEOF IDENT 
    { { annot = TBool; expr = InstanceOf($1, $3); loc = fst $loc } }
|expr INSTANCEOF LPAR IDENT RPAR { { annot = TBool; expr = InstanceOf($1, $4); loc = fst $loc } }
;

%inline list_array : 
| LBRACKET expr RBRACKET {$2}

;
mem:
| IDENT { Var($1) }
| expr DOT IDENT { Field($1, $3) }
| IDENT nonempty_list(list_array) {ArrayAccess($1, $2)}
;

