(* analyzer/lib/analysis.ml *)
open Ast

module TaintSet = Set.Make(String)

type vulnerability = { sink_var: string; message: string; }
type report = vulnerability list

let debug_mode = false
let debug_print msg = if debug_mode then print_endline ("  [DEBUG] " ^ msg)

let rec is_expr_tainted (expr: expression) (tainted_vars: TaintSet.t) : bool =
  match expr with
  | Identifier(name) -> TaintSet.mem name tainted_vars
  | CallExpr(callee, _) ->
      (match callee with
      | Identifier "fetch" ->
          debug_print "Found Source: fetch(...)";
          true
      | MemberExpr(_, Identifier "text") ->
          debug_print "Found Source: response.text()";
          true
      | MemberExpr(_, Identifier("getAttribute")) -> 
          debug_print "Found Source: getAttribute(...)"; true
      | MemberExpr(_, Identifier("get")) -> true 
      | _ -> false) 
  | TemplateLiteral(exprs) -> List.exists (fun e -> is_expr_tainted e tainted_vars) exprs
  | BinaryExpr(_, l, r) -> (is_expr_tainted l tainted_vars) || (is_expr_tainted r tainted_vars)
  | MemberExpr(obj, _) -> is_expr_tainted obj tainted_vars
  | _ -> false

let rec find_inner_scopes (expr: expression) : statement list list =
  match expr with
  | FuncExpr(_, body) -> [body] 
  | CallExpr(callee, args) -> (find_inner_scopes callee) @ (List.concat (List.map find_inner_scopes args))
  | MemberExpr(obj, prop) -> (find_inner_scopes obj) @ (find_inner_scopes prop)
  | BinaryExpr(_, l, r) -> (find_inner_scopes l) @ (find_inner_scopes r)
  | _ -> []

  let rec find_then_callbacks (expr: expression)
  : (string list * statement list) list =
  match expr with
  (* 匹配：obj.then((params) => { body }) *)
  | CallExpr( MemberExpr(_, Identifier "then"),
              [ FuncExpr(params, body) ] ) ->
      [ (params, body) ]

  (* 更复杂的表达式：例如 obj.then(cb1).then(cb2) *)
  | CallExpr(callee, args) ->
      find_then_callbacks callee
      @ List.concat (List.map find_then_callbacks args)

  | MemberExpr(obj, prop) ->
      find_then_callbacks obj @ find_then_callbacks prop

  | BinaryExpr(_, l, r) ->
      find_then_callbacks l @ find_then_callbacks r

  | TemplateLiteral exprs ->
      List.concat (List.map find_then_callbacks exprs)

  | _ ->
      []

let rec analyze_stmt (stmt: statement) (tainted_vars: TaintSet.t) : (TaintSet.t * report) =
  
  let check_inner_scopes expr =
    (* 1⃣️ 优先：处理 .then(...) 回调，把参数视为 tainted *)
    let then_cbs = find_then_callbacks expr in
    let (_, vulns_from_then) =
      List.fold_left
        (fun (t_acc, v_acc) (params, body) ->
           (* 把回调的参数加入当前 taint 集合 *)
           let t_with_params =
             List.fold_left (fun s p -> TaintSet.add p s) t_acc params
           in
           let (t', v') = analyze_block body t_with_params in
           (t', v_acc @ v'))
        (tainted_vars, [])
        then_cbs
    in

    (* 2⃣️ 其次：处理所有嵌套的 FuncExpr（不额外 taint 参数） *)
    let scopes = find_inner_scopes expr in
    if List.length scopes > 0 then
      debug_print
        (Printf.sprintf "Entering nested function scope (%d functions found)..."
           (List.length scopes));

    let vulns_from_scopes =
      List.concat
        (List.map
           (fun stmts ->
             let (_, vulns) = analyze_block stmts tainted_vars in
             vulns)
           scopes)
    in

    (* 最终合并两类结果 *)
    vulns_from_then @ vulns_from_scopes
  in

  match stmt with
  | VarDecl(decls) ->
      List.fold_left (fun (acc_taints, acc_vulns) (name, expr) ->
         let inner_vulns = check_inner_scopes expr in
         let new_taints = 
           if is_expr_tainted expr acc_taints then begin
             debug_print (Printf.sprintf "TAINTED: Variable '%s' is now tainted!" name);
             TaintSet.add name acc_taints 
           end else acc_taints 
         in
         (new_taints, acc_vulns @ inner_vulns)
      ) (tainted_vars, []) decls

  | Assignment(left, right, loc) ->
      let inner_vulns = check_inner_scopes right in
      let sink_vulns = 
        match left with
        | MemberExpr(_, Identifier("innerHTML")) ->
            debug_print "Checking Sink: innerHTML...";
            if is_expr_tainted right tainted_vars then begin
              debug_print "🚨 CRITICAL!";
              (* 将位置信息加入报告 *)
              [{ sink_var = "innerHTML"; message = (Printf.sprintf "Vulnerability at %s" loc) }]
            end else []
        | MemberExpr(_, Identifier("dangerouslySetInnerHTML")) ->
             [{ sink_var = "dangerouslySetInnerHTML"; message = "React dangerous sink detected!" }]
        | _ -> []
      in
      let new_taints =
        match left with
        | Identifier(name) ->
            if is_expr_tainted right tainted_vars then begin
               debug_print (Printf.sprintf "TAINTED: Variable '%s' infected by assignment" name);
               TaintSet.add name tainted_vars 
            end else tainted_vars
        | _ -> tainted_vars
      in
      (new_taints, inner_vulns @ sink_vulns)

  | ExprStmt(expr) -> (tainted_vars, check_inner_scopes expr)
  | Block(stmts) -> analyze_block stmts tainted_vars
  | FuncDecl(name, _, body) -> 
      debug_print (Printf.sprintf "Analyzing function declaration: %s" name);
      analyze_block body tainted_vars
  
  (* 关键修复：分析控制流 *)
  | IfStmt(cond, then_stmt, else_stmt_opt) ->
      debug_print "Analyzing IF statement...";
      let inner_vulns = check_inner_scopes cond in
      let (taints1, vulns1) = analyze_stmt then_stmt tainted_vars in
      let (taints2, vulns2) = match else_stmt_opt with
        | Some s -> analyze_stmt s tainted_vars
        | None -> (tainted_vars, [])
      in
      (* 合并污点：保守策略，假设两个分支都可能执行，取并集 *)
      let final_taints = TaintSet.union taints1 taints2 in
      (final_taints, inner_vulns @ vulns1 @ vulns2)

  | LoopStmt(body) ->
      debug_print "Analyzing LOOP statement...";
      (* 简单处理循环：只分析一次 body *)
      analyze_stmt body tainted_vars

  | _ -> (tainted_vars, [])

and analyze_block (stmts: statement list) (initial_taints: TaintSet.t) : (TaintSet.t * report) =
  List.fold_left (fun (current_taints, all_vulns) stmt ->
    let (new_taints, new_vulns) = analyze_stmt stmt current_taints in
    (new_taints, all_vulns @ new_vulns)
  ) (initial_taints, []) stmts