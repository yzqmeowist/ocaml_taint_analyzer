(* convert JSON AST *)

open Yojson.Basic.Util
open Ast  

(* 安全地将 JSON 值转换为字符串，如果转换失败则返回默认值 *)
(* 参数：
   - default: 转换失败时返回的默认字符串
   - json: 要转换的 JSON 值
   返回：字符串值 *)
let json_to_string default json =
  try to_string json with Type_error _ -> default

(* 将 JSON 对象转换为 AST 表达式 *)
(* 这个函数根据 JSON 中的 "type" 字段识别表达式类型，并递归地解析子表达式 *)
(* 参数：json - 表示表达式的 JSON 对象
   返回：对应的 AST 表达式 *)
let rec parse_exprs json_list = 
  List.map json_to_expr json_list
and json_to_expr json =
  (* 从 JSON 中提取 type 字段，用于判断表达式类型 *)
  let type_ = json |> member "type" |> to_string_option |> Option.value ~default:"Unknown" in
  match type_ with
  (* 标识符：如变量名、函数名 *)
  | "Identifier" -> Identifier (json |> member "name" |> json_to_string "?")
  
  (* 字面量：如字符串、数字等 *)
  | "Literal" -> StringLiteral (json |> member "raw" |> json_to_string "")
  
  (* 模板字符串：如 `hello ${name}` *)
  | "TemplateLiteral" -> TemplateLiteral (json |> member "expressions" |> to_list |> parse_exprs)
  
  (* 函数调用表达式：如 foo(a, b, c) *)
  | "CallExpression" ->
      (* 解析被调用的函数（callee） *)
      let callee = json |> member "callee" |> json_to_expr in
      (* 解析参数列表 *)
      let args = match json |> member "arguments" with `List l -> parse_exprs l | _ -> [] in
      CallExpr (callee, args)
  
  (* 成员访问表达式：如 obj.prop 或 arr[index] *)
  | "MemberExpression" ->
      MemberExpr (json |> member "object" |> json_to_expr, json |> member "property" |> json_to_expr)
  
  (* 函数表达式：箭头函数 (a, b) => {...} 或匿名函数 function(a, b) {...} *)
  | "ArrowFunctionExpression" | "FunctionExpression" ->
      (* 获取函数体节点 *)
      let body_node = json |> member "body" in
      let body_type = body_node |> member "type" |> to_string_option |> Option.value ~default:"" in
      (* 判断函数体是代码块还是单个表达式 *)
      let stmts = 
        if body_type = "BlockStatement" then
          (* 代码块形式：{ stmt1; stmt2; ... } *)
          body_node |> member "body" |> to_list |> List.map json_to_stmt
        else 
          (* 单表达式形式（箭头函数简写）：() => expr *)
          [ExprStmt (json_to_expr body_node)]
      in
      (* 解析函数参数 *)
      let parse_param p = 
        match p |> member "type" |> to_string_option with
        | Some "Identifier" -> p |> member "name" |> json_to_string "_"
        | _ -> "_complex_param"  (* 复杂参数（如解构）暂时用占位符 *)
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
      (* 解析单个变量声明 *)
      let parse_decl d =
        (* 提取变量名（简单标识符）或用占位符表示复杂的解构模式 *)
        let id = match d |> member "id" |> member "type" |> to_string_option with
          | Some "Identifier" -> d |> member "id" |> member "name" |> json_to_string "_"
          | _ -> "_complex_var"  (* 解构赋值等复杂模式 *)
        in
        (* 提取初始化表达式 *)
        let init = d |> member "init" in
        let expr = if init = `Null then OtherExpr else json_to_expr init in
        (id, expr)
      in
      VarDecl (List.map parse_decl decls)
  
  (* 表达式语句：将表达式作为语句执行 *)
  | "ExpressionStatement" ->
      let expr = json |> member "expression" in
      let e_type = expr |> member "type" |> to_string_option |> Option.value ~default:"" in
      if e_type = "AssignmentExpression" then
        let left = expr |> member "left" |> json_to_expr in
        let right = expr |> member "right" |> json_to_expr in
        (* 读取注入的位置信息 *)
        let loc = expr |> member "_source_loc" |> json_to_string "unknown:0" in
        Assignment (left, right, loc)
      else
        ExprStmt (json_to_expr expr)
  
  (* 代码块语句：{ stmt1; stmt2; ... } *)
  | "BlockStatement" -> Block (json |> member "body" |> to_list |> List.map json_to_stmt)
  
  (* 函数声明：function foo(params) { body } *)
  | "FunctionDeclaration" ->
      (* 提取函数名 *)
      let id = json |> member "id" |> member "name" |> json_to_string "anon" in
      (* 提取函数体（语句列表） *)
      let body = json |> member "body" |> member "body" |> to_list |> List.map json_to_stmt in
      (* 注意：这里暂时将参数列表设为空 []，实际应该解析 params 字段 *)
      FuncDecl (id, [], body)
  
  (* if 条件语句：if (test) { consequent } else { alternate } *)
  | "IfStatement" ->
      (* 提取条件表达式 *)
      let test = json |> member "test" |> json_to_expr in
      (* 提取 then 分支 *)
      let consequent = json |> member "consequent" |> json_to_stmt in
      (* 提取可选的 else 分支 *)
      let alternate_json = json |> member "alternate" in
      let alternate = 
        if alternate_json = `Null then None  (* 没有 else 分支 *)
        else Some (json_to_stmt alternate_json) 
      in
      IfStmt (test, consequent, alternate)

  (* 循环语句：while, do-while, for, for-in, for-of *)
  (* 这里简化处理，只保存循环体，忽略循环条件和迭代器 *)
  | "WhileStatement" | "DoWhileStatement" | "ForStatement" | "ForInStatement" | "ForOfStatement" ->
      let body = json |> member "body" |> json_to_stmt in
      LoopStmt (body)

  (* return  *)
  | "ReturnStatement" ->
      let arg = json |> member "argument" in
      (* 如果没有返回值，用 OtherStmt 表示；否则用 ExprStmt 包装返回表达式 *)
      if arg = `Null then OtherStmt else ExprStmt (json_to_expr arg)
  
  | _ -> OtherStmt

(* parse JS code *)
let parse_program json_file =
  (* read JSON file *)
  let json = Yojson.Basic.from_file json_file in
    let body = json |> member "body" |> to_list in
      List.map json_to_stmt body