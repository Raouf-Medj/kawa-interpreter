%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA DOT SET
%token VAR ATTR METHOD CLASS NEW THIS EXTENDS
%token TINT TBOOL TVOID
%token TRUE FALSE
%token IF ELSE WHILE RETURN
%token ADD DIV SUB MUL REM OR AND NOT
%token EQ NEQ LT LE GT GE
%token PRINT
%token EOF
%token FINAL
%token INSTANCEOF

// %right SET
%left OR
%left AND
%left EQ NEQ
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
        if has_duplicates (List.map fst glb) then failwith "Duplicate variable declaration"
        else glb); 
      main
    } }
;

class_def:
| CLASS IDENT opt_parent BEGIN list(attr_decl) list(method_def) END { { 
    class_name = $2;
    attributes = List.map (fun (id, typ, _) -> (id, typ)) $5;
    methods = $6;
    parent = $3;
    is_attr_final = List.map (fun (id, _, is_final) -> (id, is_final)) $5;
  } }
;

var_decl:
| VAR typp separated_nonempty_list(COMMA, IDENT) SEMI { List.map (fun ident -> (ident, $2)) $3 }
;

attr_decl:
| ATTR typp IDENT SEMI { ($3, $2, false) }
| ATTR FINAL typp IDENT SEMI { ($4, $3, true) }
;

param_decl:
| typp IDENT { ($2, $1) }
;

method_def:
| METHOD tp=typp id=IDENT LPAR param_lst=separated_list(COMMA, param_decl) RPAR BEGIN locs=list(var_decl) sequence=list(instr) END { {
    method_name = id;
    code = sequence;
    params = param_lst;
    locals = 
      (let glb = List.fold_left (fun acc l -> acc @ l) [] locs in
      let has_duplicates lst =
        let tbl = Hashtbl.create (List.length lst) in
        List.fold_left (fun found x ->
          if found then true
          else if Hashtbl.mem tbl x then true
          else (Hashtbl.add tbl x (); false)
        ) false lst in
      if has_duplicates (List.map fst glb) then failwith "Duplicate variable declaration"
      else glb);
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

instr:
| PRINT LPAR e=expr RPAR SEMI { Print(e) }
| mem SET expr SEMI { Set($1, $3) }
| IF LPAR e=expr RPAR BEGIN b1=list(instr) END ELSE BEGIN b2=list(instr) END { If(e, b1, b2) }
| WHILE LPAR e=expr RPAR BEGIN b=list(instr) END { While(e, b) }
| RETURN expr SEMI { Return($2) }
| expr SEMI { Expr($1) }
;

expr:
| INT { Int($1) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| THIS { This }
| mem { Get($1) }
| expr ADD expr { Binop(Add, $1, $3) }
| expr SUB expr { Binop(Sub, $1, $3) }
| expr MUL expr { Binop(Mul, $1, $3) }
| expr DIV expr { Binop(Div, $1, $3) }
| expr REM expr { Binop(Rem, $1, $3) }
| expr LT expr { Binop(Lt, $1, $3) }
| expr LE expr { Binop(Le, $1, $3) }
| expr GT expr { Binop(Gt, $1, $3) }
| expr GE expr { Binop(Ge, $1, $3) }
| expr EQ expr { Binop(Eq, $1, $3) }
| expr NEQ expr { Binop(Neq, $1, $3) }
| expr AND expr { Binop(And, $1, $3) }
| expr OR expr { Binop(Or, $1, $3) }
| expr INSTANCEOF IDENT { InstanceOf($1, $3) }
| SUB expr %prec NEG { Unop(Opp, $2) }
| NOT expr { Unop(Not, $2) }
| LPAR expr RPAR { $2 }
| NEW IDENT { New($2) }
| NEW IDENT LPAR separated_list(COMMA, expr) RPAR { NewCstr($2, $4) }
| expr DOT IDENT LPAR separated_list(COMMA, expr) RPAR { MethCall($1, $3, $5) }
;

mem:
| IDENT { Var($1) }
| expr DOT IDENT { Field($1, $3) }
;

