{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "print",      PRINT;
        "main",       MAIN;
        "true",       TRUE;
        "false",      FALSE;
        "var",        VAR;
        "attribute",  ATTR;
        "method",     METHOD;
        "class",      CLASS;
        "new",        NEW;
        "this",       THIS;
        "if",         IF;
        "else",       ELSE;
        "while",      WHILE;
        "return",     RETURN;
        "int",        TINT;
        "bool",       TBOOL;
        "void",       TVOID;
        "extends",    EXTENDS;
        "final",      FINAL;
        "static",     STATIC;
        "instanceof", INSTANCEOF;
        "super",      SUPER;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
          
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }

  | ";"  { SEMI }
  | ","  { COMMA }
  | "."  { DOT }
  | "="  { SET }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "||" { OR }
  | "&&" { AND }
  | "!"  { NOT }
  | "==" { EQ }
  | "%"  { REM }
  | "!=" { NEQ }
  | "<"  { LT }
  | "<=" { LE }
  | ">"  { GT }
  | ">=" { GE }
  | "+"  { ADD }
  | "/"  { DIV }
  | "-"  { SUB }
  | "*"  { MUL }
  | "===" {STRUCTEG}
  | "=/=" {STRUCTINEG}
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }

