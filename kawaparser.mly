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


%start program
%type <Kawa.program> program

%%

program:
| vrs=list(var_decl) cls=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    { {classes=cls; globals=vrs; main} }
;

class_def:
| CLASS IDENT opt_parent BEGIN list(attr_decl) list(method_def) END { { 
    class_name = $2;
    attributes = $5;
    methods = $6;
    parent = $3;
  } }
;

var_decl:
| VAR typp IDENT SEMI { ($3, $2) }
;

attr_decl:
| ATTR typp IDENT SEMI { ($3, $2) }
;

param_decl:
| typp IDENT { ($2, $1) }
;

method_def:
| METHOD tp=typp id=IDENT LPAR param_lst=separated_list(COMMA, param_decl) RPAR BEGIN locs=list(var_decl) sequence=list(instruction) END { {
    method_name = id;
    code = sequence;
    params = param_lst;
    locals = locs;
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

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| mem SET expression SEMI { Set($1, $3) }
| IF LPAR e=expression RPAR BEGIN b1=list(instruction) END ELSE BEGIN b2=list(instruction) END { If(e, b1, b2) }
| WHILE LPAR e=expression RPAR BEGIN b=list(instruction) END { While(e, b) }
| RETURN expression SEMI { Return($2) }
| expression SEMI { Expr($1) }
;

expression:
| n=INT   { Int(n) }
| TRUE    { Bool(true) }
| FALSE   { Bool(false) }
| THIS    { This }
| mem     { Get($1) }
| expression bop expression    { Binop($2, $1, $3) }
| uop expression    { Unop($1, $2) }
| LPAR e=expression RPAR    { e }
| NEW id=IDENT    { New(id) }
| NEW IDENT LPAR list(expression) RPAR     {NewCstr($2, $4)}
| expression DOT IDENT LPAR list(expression) RPAR     {MethCall($1, $3, $5)}
;

mem:
| id=IDENT    { Var(id) }
| e=expression DOT id=IDENT    { Field(e, id) }
;

uop:
| SUB { Opp }
| NOT { Not }
;

bop:
| ADD { Add }
| SUB { Sub }
| MUL { Mul }
| DIV { Div }
| REM { Rem }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| EQ { Eq }
| NEQ { Neq }
| AND { And }
| OR { Or }
;
