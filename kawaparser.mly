%{

  open Lexing
  open Kawa

  let rec tarray_maker d t= if(d=1) then TArray t else TArray (tarray_maker (d-1) t)  
  let rec flatten lst =
    match lst with
    | [] -> []
    | hd :: tl -> hd @ (flatten tl)
%}


%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI COMMA DOT SET RBRACKET(* [ *) LBRACKET(* ] *)
%token VAR ATTR METHOD CLASS NEW THIS EXTENDS
%token TINT TBOOL TVOID
%token TRUE FALSE
%token IF ELSE WHILE RETURN
%token ADD DIV SUB MUL REM OR AND NOT INSTANCEOF
%token EQ NEQ LT LE GT GE STRUCTEG STRUCTINEG
%token PRINT
%token EOF


// %right SET
%left OR
%left AND
%left EQ NEQ
%left STRUCTEG STRUCTINEG
%left INSTANCEOF
%left LT LE GT GE
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
    { {classes=cls; globals=(flatten vrs); main} }
;

class_def:
| CLASS IDENT opt_parent BEGIN attr=list(attr_decl) list(method_def) END { { 
    class_name = $2;
    attributes = (flatten attr);
    methods = $6;
    parent = $3;
  } }
;

var_decl:
| VAR typpc list(suite) init SEMI { List.map (fun x -> (x,$2,$4)) $3 }
;

attr_decl:
| ATTR typpc list(suite) init SEMI { List.map (fun x -> (x,$2,$4)) $3}
;

init:
| SET expr {Some($2)}
|           {None}

suite : 
| IDENT COMMA {$1}
| IDENT {$1}
;


param_decl:
| typpc IDENT init{ ($2, $1, $3) }
;

method_def:
| METHOD tp=typpc id=IDENT LPAR param_lst=separated_list(COMMA, param_decl) RPAR BEGIN locs=list(var_decl) sequence=list(instr) END { {
    method_name = id;
    code = sequence;
    params = param_lst;
    locals = (flatten locs);
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
| expr STRUCTEG expr { Binop(Structeg, $1, $3) }
| expr STRUCTINEG expr { Binop(Structineg, $1, $3) }
| SUB expr %prec NEG { Unop(Opp, $2) }
| NOT expr { Unop(Not, $2) }
| LPAR expr RPAR { $2 }
| NEW IDENT { New($2) }
| NEW IDENT LPAR separated_list(COMMA, expr) RPAR { NewCstr($2, $4) }
| expr DOT IDENT LPAR separated_list(COMMA, expr) RPAR { MethCall($1, $3, $5) }
| NEW typp nonempty_list(list_array) { EArrayCreate($2, $3) }
| e = expr INSTANCEOF LPAR i = IDENT RPAR {InstanceOf(e , i)}


;
%inline list_array : 
| LBRACKET expr RBRACKET {$2}

;
mem:
| IDENT { Var($1) }
| expr DOT IDENT { Field($1, $3) }
| IDENT nonempty_list(list_array) {ArrayAccess($1, $2)}

;

