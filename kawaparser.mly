%{

  open Lexing
  open Kawa

  let rec tarray_maker d t= if(d=1) then TArray t else TArray (tarray_maker (d-1) t)

  let has_duplicates lst =
    let tbl = Hashtbl.create (List.length lst) in
    List.fold_left (fun found x ->
      if found then true
      else if Hashtbl.mem tbl x then true
      else (Hashtbl.add tbl x (); false)
    ) false lst

  let flatten lst = List.fold_left (fun acc l -> acc @ l) [] lst

%}


%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA DOT SET RBRACKET LBRACKET
%token VAR ATTR METHOD CLASS NEW THIS EXTENDS
%token TINT TBOOL TVOID
%token TRUE FALSE
%token IF ELSE WHILE RETURN
%token ADD DIV SUB MUL REM OR AND NOT
%token EQ NEQ LT LE GT GE STRUCTEG STRUCTINEG
%token PRINT
%token EOF
%token FINAL INSTANCEOF SUPER STATIC


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
        (let glb = flatten vrs in
        if has_duplicates (List.map (fun (id, _, _) -> id) glb) then failwith "Duplicate variable declaration"
        else List.map (fun (id, ty, _) -> (id, ty)) glb); 
      globals_init_vals = 
        (let glb = flatten vrs in
        List.map (fun (id, _, init) -> (id, init)) glb);
      main
    } }
;

class_def:
| CLASS IDENT opt_parent BEGIN list(attr_decl) list(method_def) END {
    {
      class_name = $2; (* Nom de la classe *)
      parent = $3; (* Classe parent, si elle existe *)
      attributes = 
        (let flat = flatten $5 in
        if has_duplicates (List.map (fun (id, _, _, _, _) -> id) flat) then failwith "Duplicate attribute declaration"
        else List.map (fun (id, typ, _, _, _) -> (id, typ)) flat); (* Liste des attributs *)
      methods = $6; (* Liste des méthodes *)
      is_attr_final = List.map (fun (id, _, is_final, _, _) -> (id, is_final)) (flatten $5); (* Finalité des attributs *)
      static_attribut = List.map (fun (id, _, _, is_static, _) -> (id, is_static)) (flatten $5); (* Attributs statiques *)
      attr_init_vals = List.map (fun (id, _, _, _, init) -> (id, init)) (flatten $5); (* Valeurs initiales des attributs *)
    }
  }
;

var_decl:
| VAR typpc separated_nonempty_list(COMMA, ident_init) SEMI { List.map (fun (ident, init) -> (ident, $2, init)) $3 }
;

attr_decl:
| ATTR typpc separated_nonempty_list(COMMA, ident_init) SEMI { List.map (fun (ident, init) -> (ident, $2, false, false, init)) $3 }
| ATTR FINAL typpc separated_nonempty_list(COMMA, ident_init) SEMI { List.map (fun (ident, init) -> (ident, $3, true, false, init)) $4 }
| ATTR STATIC typpc separated_nonempty_list(COMMA, ident_init) SEMI { List.map (fun (ident, init) -> (ident, $3, false, true, init)) $4 }
| ATTR STATIC FINAL typpc separated_nonempty_list(COMMA, ident_init) SEMI 
    | ATTR FINAL STATIC typpc separated_nonempty_list(COMMA, ident_init) SEMI 
    { List.map (fun (ident, init) -> 
        match init with
        | Some v -> (ident, $4, true, true, Some(v))
        | None -> failwith "Static final attributes must be initialized"
      ) $5 }
;

ident_init:
| IDENT { ($1, None) }  (* IDENT sans initialisation *)
| IDENT SET expr { ($1, Some($3)) }  (* IDENT avec initialisation *)
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
      (let loc = flatten locs in
      if has_duplicates (List.map (fun (id, _, _) -> id) loc) then failwith "Duplicate variable declaration"
      else List.map (fun (id, ty, _) -> (id, ty)) loc); 
    locals_init_vals =
      (let loc = flatten locs in
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
;

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
| INT { Int($1) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| THIS { This }
| SUPER { Super }
| mem { Get($1) }
| LPAR expr RPAR { $2 }
| SUB expr %prec NEG { Unop(Opp, $2) }
| NOT expr { Unop(Not, $2) }
| expr bop expr { Binop($2, $1, $3) }
| expr INSTANCEOF IDENT { InstanceOf($1, $3) }
| expr INSTANCEOF LPAR IDENT RPAR {InstanceOf($1 , $4)}
| NEW IDENT { New($2) }
| NEW IDENT LPAR separated_list(COMMA, expr) RPAR { NewCstr($2, $4) }
| expr DOT IDENT LPAR separated_list(COMMA, expr) RPAR { MethCall($1, $3, $5) }
| NEW typp nonempty_list(list_array) { EArrayCreate($2, $3) }
;

%inline bop:
| ADD { Add }
| SUB { Sub }
| MUL { Mul}
| DIV { Div }
| REM { Rem } 
| LT { Lt } 
| LE { Le }
| GT { Gt }
| GE { Ge }
| EQ { Eq }
| NEQ { Neq }
| AND { And }
| OR {Or}
| STRUCTEG { Structeg }
| STRUCTINEG { Structineg }
;

%inline list_array : 
| LBRACKET expr RBRACKET {$2}
;

mem:
| IDENT { Var($1) }
| expr DOT IDENT { Field($1, $3) }
| IDENT nonempty_list(list_array) {ArrayAccess($1, $2)}
;

