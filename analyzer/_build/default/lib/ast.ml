(* AST Definition for JavaScript *)

type operator = string

type expression =
  | Identifier of string                              (* names of variables and functions *)
  | MemberExpr of expression * expression             (* members *)
  | CallExpr of expression * expression list          (* function call *)
  | StringLiteral of string                           (* string literal *)
  | TemplateLiteral of expression list                (* template string like ${name} *)
  | BinaryExpr of operator * expression * expression  (* binary expression *)
  | FuncExpr of string list * statement list          (* function *)
  | OtherExpr                                         

and statement =
  | VarDecl of (string * expression) list             (* variation *)          
  | ExprStmt of expression                            (* expressions *)
  | Block of statement list                           
  | FuncDecl of string * string list * statement list (* function name, argument list, function body *)
  | IfStmt of expression * statement * statement option (* if, condition, else, then *)
  | LoopStmt of statement                             (* for/while *)
  | Assignment of expression * expression * string
  | OtherStmt                                         

type program = statement list