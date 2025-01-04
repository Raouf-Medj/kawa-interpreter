%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI VIRG PT
%token VAR ATTR METHOD CLASS NEW THIS
%token TINT TBOOL TVOID
%token TRUE FALSE
%token IF ELSE WHILE RETURN
%token ADD DIV SUB MUL OR AND NOT
%token EQ EQS NEQ LT LE GT GE
%token PRINT
%token EOF


%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
;

expression:
| n=INT   { Int(n) }
| TRUE    { Bool(true) }
| FALSE   { Bool(false) }
;
