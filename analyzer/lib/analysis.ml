open Ast

(* store the names of tainted variables *)
module TaintSet = Set.Make(String)

type vulnerability = { sink_var: string; message: string; }
type report = vulnerability list

(* all possible tainted expressions *)
let rec is_expr_tainted (expr: expression) (tainted_vars: TaintSet.t) : bool =
  match expr with
  | Identifier(name) -> TaintSet.mem name tainted_vars
  | CallExpr(callee, _) ->
      (match callee with
      | Identifier "fetch" ->
          true
      | MemberExpr(_, Identifier "text") ->
          true
      | MemberExpr(_, Identifier("getAttribute")) -> 
         true
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

  (* find the callbacks in Promise structure *)
  let rec find_then_callbacks (expr: expression)
  : (string list * statement list) list =
  match expr with
  (* obj.then((params) => { body }) *)
  | CallExpr( MemberExpr(_, Identifier "then"),
              [ FuncExpr(params, body) ] ) ->
      [ (params, body) ]

  (* obj.then(cb1).then(cb2)... *)
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

(* analyze a simple statement *)
let rec analyze_stmt (stmt: statement) (tainted_vars: TaintSet.t) : (TaintSet.t * report) =
  let check_inner_scopes expr =
    (* .then(...) *)
    let then_cbs = find_then_callbacks expr in
    let (_, vulns_from_then) =
      List.fold_left
        (fun (t_acc, v_acc) (params, body) ->
           let t_with_params =
             List.fold_left (fun s p -> TaintSet.add p s) t_acc params
           in
           let (t', v') = analyze_block body t_with_params in
           (t', v_acc @ v'))
        (tainted_vars, [])
        then_cbs
    in

    (* FuncExpr *)
    let scopes = find_inner_scopes expr in
      let vulns_from_scopes =
      List.concat
        (List.map
           (fun stmts ->
             let (_, vulns) = analyze_block stmts tainted_vars in
             vulns)
           scopes)
    in

    (* merge results *)
    vulns_from_then @ vulns_from_scopes
  in

  match stmt with
  (* let declaration *)
  | VarDecl(decls) ->
      List.fold_left (fun (acc_taints, acc_vulns) (name, expr) ->
         let inner_vulns = check_inner_scopes expr in
         let new_taints = 
           if is_expr_tainted expr acc_taints then begin
             TaintSet.add name acc_taints 
           end else acc_taints 
         in
         (new_taints, acc_vulns @ inner_vulns)
      ) (tainted_vars, []) decls

  (* assignment *)
  | Assignment(left, right, loc) ->
      let inner_vulns = check_inner_scopes right in
      let sink_vulns = 
        match left with
        | MemberExpr(_, Identifier("innerHTML")) -> (* element.innerHTML = tainted *)
            if is_expr_tainted right tainted_vars then begin
              (* add location message *)
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
               TaintSet.add name tainted_vars 
            end else tainted_vars
        | _ -> tainted_vars
      in
      (new_taints, inner_vulns @ sink_vulns)

  | ExprStmt(expr) -> (tainted_vars, check_inner_scopes expr)
  | Block(stmts) -> analyze_block stmts tainted_vars
  | FuncDecl(_, _, body) -> 
      analyze_block body tainted_vars
  | IfStmt(cond, then_stmt, else_stmt_opt) ->
      let inner_vulns = check_inner_scopes cond in
      let (taints1, vulns1) = analyze_stmt then_stmt tainted_vars in
      let (taints2, vulns2) = match else_stmt_opt with
        | Some s -> analyze_stmt s tainted_vars
        | None -> (tainted_vars, [])
      in
      let final_taints = TaintSet.union taints1 taints2 in
      (final_taints, inner_vulns @ vulns1 @ vulns2)

  | LoopStmt(body) ->
      analyze_stmt body tainted_vars

  | _ -> (tainted_vars, [])

and analyze_block (stmts: statement list) (initial_taints: TaintSet.t) : (TaintSet.t * report) =
  List.fold_left (fun (current_taints, all_vulns) stmt ->
    let (new_taints, new_vulns) = analyze_stmt stmt current_taints in
    (new_taints, all_vulns @ new_vulns)
  ) (initial_taints, []) stmts