(* convert JSON AST into OCaml Ast *)

open Yojson.Basic.Util
open Ast  

(* extract strings otherwise return default *)
let json_to_string default json =
  try to_string json with Type_error _ -> default

(* recursively convert JSON lists into expression lists and convert individual JSON objects into expressions *)
let rec parse_exprs json_list = 
  List.map json_to_expr json_list
and json_to_expr json =
  let type_ = json |> member "type" |> to_string_option |> Option.value ~default:"Unknown" in
  match type_ with
  (* identifiers: function name, variable name, ... *)
  | "Identifier" -> Identifier (json |> member "name" |> json_to_string "?")
  
  (* string literal *)
  | "Literal" -> StringLiteral (json |> member "raw" |> json_to_string "")
  
  (* template literal: `hello ${name}` *)
  | "TemplateLiteral" -> TemplateLiteral (json |> member "expressions" |> to_list |> parse_exprs)
  
  (* calling expression: f(a, b, ...) *)
  | "CallExpression" ->
      (* 解析被调用的函数（callee） *)
      let callee = json |> member "callee" |> json_to_expr in
      (* 解析参数列表 *)
      let args = match json |> member "arguments" with `List l -> parse_exprs l | _ -> [] in
      CallExpr (callee, args)
  
  (* member expressions: obj.prop *)
  | "MemberExpression" ->
      MemberExpr (json |> member "object" |> json_to_expr, json |> member "property" |> json_to_expr)
  
  (* function expressions: (a, b) => {...} or function(a, b) {...} *)
  | "ArrowFunctionExpression" | "FunctionExpression" ->
      let body_node = json |> member "body" in
      let body_type = body_node |> member "type" |> to_string_option |> Option.value ~default:"" in
      let stmts = 
        if body_type = "BlockStatement" then
          (* { stmt1; stmt2; ... } *)
          body_node |> member "body" |> to_list |> List.map json_to_stmt
        else 
          (* () => expr *)
          [ExprStmt (json_to_expr body_node)]
      in
      (* parse function parameters*)
      let parse_param p = 
        match p |> member "type" |> to_string_option with
        | Some "Identifier" -> p |> member "name" |> json_to_string "_"
        | _ -> "_complex_param"  (* other kinds of parameters *)
      in
      let params = json |> member "params" |> to_list |> List.map parse_param in
      FuncExpr (params, stmts)

  | _ -> OtherExpr

(* convert json object to statements with the field "type" *)
and json_to_stmt json =
  let type_ = json |> member "type" |> to_string_option |> Option.value ~default:"Unknown" in
  match type_ with
  (* variable declaration *)
  | "VariableDeclaration" ->
      let decls = json |> member "declarations" |> to_list in
      let parse_decl d =
        let id = match d |> member "id" |> member "type" |> to_string_option with
          | Some "Identifier" -> d |> member "id" |> member "name" |> json_to_string "_"
          (*  *)
          | _ -> "_complex_var"  
        in
        let init = d |> member "init" in
        let expr = if init = `Null then OtherExpr else json_to_expr init in
        (id, expr)
      in
      VarDecl (List.map parse_decl decls)
  
  (* expressions: function call and assignment *)
  | "ExpressionStatement" ->
      let expr = json |> member "expression" in
      let e_type = expr |> member "type" |> to_string_option |> Option.value ~default:"" in
      if e_type = "AssignmentExpression" then
        let left = expr |> member "left" |> json_to_expr in
        let right = expr |> member "right" |> json_to_expr in
        (* location mark *)
        let loc = expr |> member "_source_loc" |> json_to_string "unknown:0" in
        Assignment (left, right, loc)
      else
        ExprStmt (json_to_expr expr)
  
  (* { stmt1; stmt2; ... } *)
  | "BlockStatement" -> Block (json |> member "body" |> to_list |> List.map json_to_stmt)
  
  (* function f(params) { body } *)
  | "FunctionDeclaration" ->
      let id = json |> member "id" |> member "name" |> json_to_string "anon" in
      let body = json |> member "body" |> member "body" |> to_list |> List.map json_to_stmt in
      FuncDecl (id, [], body)
  
  (* if-else structure *)
  | "IfStatement" ->
      let test = json |> member "test" |> json_to_expr in
      let consequent = json |> member "consequent" |> json_to_stmt in
      let alternate_json = json |> member "alternate" in
      let alternate = 
        if alternate_json = `Null then None  (* no else *)
        else Some (json_to_stmt alternate_json) 
      in
      IfStmt (test, consequent, alternate)

  (* loop *)
  | "WhileStatement" | "DoWhileStatement" | "ForStatement" | "ForInStatement" | "ForOfStatement" ->
      let body = json |> member "body" |> json_to_stmt in
      LoopStmt (body)

  (* return *)
  | "ReturnStatement" ->
      let arg = json |> member "argument" in
      if arg = `Null then OtherStmt else ExprStmt (json_to_expr arg)
  
  | _ -> OtherStmt

(* parse JS code *)
let parse_program json_file =
  let json = Yojson.Basic.from_file json_file in
    let body = json |> member "body" |> to_list in
      List.map json_to_stmt body