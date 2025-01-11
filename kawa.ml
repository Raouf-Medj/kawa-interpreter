(**
   Kawa : un petit langage à objets inspiré de Java
 *)

(* Types déclarés pour les attributs, pour les variables, et pour les 
   paramètres et résultats des méthodes. *)
type typ =
  | TVoid
  | TInt
  | TBool
  | TClass of string
  | TArray of typ 


let rec typ_to_string = function
  | TVoid    -> "void"
  | TInt     -> "int"
  | TBool    -> "bool"
  | TClass c -> c
  | TArray a -> Printf.sprintf "%s[]" (typ_to_string a)


type unop  = Opp | Not
type binop = Add | Sub | Mul | Div | Rem
           | Lt  | Le  | Gt | Ge | Eq  | Neq
           | And | Or 
           | Structeg |Structineg

(* Expressions *)
type expr =
  (* Base arithmétique *)
  | Int    of int
  | Bool   of bool
  | Unop   of unop * expr
  | Binop  of binop * expr * expr
  (* Accès à une variable ou un attribut *)
  | Get      of mem_access
  (* Objet courant *)
  | This
  (* Création d'un nouvel objet *)
  | New      of string
  | NewCstr  of string * expr list
  (*Check si c'est une instance*)
  | InstanceOf of expr * string
  (* Appel de méthode *)
  | MethCall of expr * string * expr list
  | EArrayCreate of typ * expr list  (* Création d'un tableau : type et taille *)

(* Accès mémoire : variable ou attribut d'un objet *)
and mem_access =
  | Var   of string
  | Field of expr (* objet *) * string (* nom d'un attribut *)
  | ArrayAccess of string (* nom tableau *) * expr list (* indice *)


(* Instructions *)
type instr =
  (* Affichage d'un entier *)
  | Print  of expr
  (* Écriture dans une variable ou un attribut *)
  | Set    of mem_access * expr
  (* Structures de contrôle usuelles *)
  | If     of expr * seq * seq
  | While  of expr * seq
  (* Fin d'une fonction *)
  | Return of expr
  (* Expression utilisée comme instruction *)
  | Expr   of expr

and seq = instr list

(* Définition de méthode 

   Syntaxe : method <type de retour> <nom> (<params>) { ... }

   Le corps de la méthode est similaire au corps d'une fonction. *)
type method_def = {
    method_name: string;
    code: seq;
    params: (string * typ*(expr option) ) list;
    locals: (string * typ *(expr option)) list;
    return: typ;
  }
        
(* Définition de classe 

   Syntaxe : class <nom de la classe> { ... }
        ou : class <nom de la classe> extends <nom de la classe mère> { ... }

   On considère que toute classe C contient une définition de méthode de nom
   "constructor" et de type de retour void, qui initialise les champs du 
   paramètre implicite this. *)
type class_def = {
    class_name: string;
    attributes: (string * typ * (expr option)) list;
    methods: method_def list;
    parent: string option;
  }

(* Programme complet : variables globales, classes, et une séquence 
   d'instructions *)
type program = {
    classes: class_def list;
    globals: (string * typ * (expr option)) list;
    main: seq;
  }
