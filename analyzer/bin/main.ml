open Analyzer_lib

(* store all vulnerabilities for display *)
let all_vulnerabilities = ref []

(* execute the command *)
let run_command cmd =
  let exit_code = Sys.command cmd in
  if exit_code <> 0 then begin
    Printf.printf "Command failed: %s\n" cmd;
    failwith "Command execution failed"
  end

let ends_with s suffix =
  let len_s = String.length s in
  let len_suf = String.length suffix in
  len_s >= len_suf && String.sub s (len_s - len_suf) len_suf = suffix

(* traverse all .ts/.tsx files *)
let rec find_files root_dir =
  try
    let items = Sys.readdir root_dir in
    Array.fold_left (fun acc item ->
      let full_path = Filename.concat root_dir item in
      if Sys.is_directory full_path then
        acc @ (find_files full_path)
      else if (ends_with item ".tsx" || ends_with item ".ts") && not (ends_with item ".d.ts") then
        acc @ [full_path]
      else
        acc
    ) [] items
  with Sys_error _ -> []

let analyze_single_file relative_path =
  Printf.printf "\nAnalyzing file: %s\n" relative_path;
  print_endline "---------------------------------------------------";

  try
    (* stage 1: preprocessing -- bundling with esbuild *)
    let esbuild_cmd = Printf.sprintf 
      "npx esbuild %s \
      --bundle \
      --platform=browser \
      --format=esm \
      --target=es2017 \
      --sourcemap \
      --outfile=bundled_map.js \
      --log-level=silent \
      --external:react \
      --external:react-dom \
      --external:react-redux \
      --external:antd \
      --external:\"next/*\" \
      --external:\"@ant-design/*\" \
      --external:\"tsparticles*\" \
      --external:react-tsparticles \
      --external:framer-motion" 
      relative_path 
    in
    run_command esbuild_cmd;

    (* stage 2: parsing *)
    run_command "node ../parser.js";

    (* stage 3 & 4: transformation & analysis *)
    let program = Parser.parse_program "ast.json" in
    
    let (_, vulns) = Analysis.analyze_block program Analysis.TaintSet.empty in
    
    if List.length vulns > 0 then begin
      Printf.printf "FOUND %d VULNERABILITIES!\n" (List.length vulns);
      all_vulnerabilities := (relative_path, vulns) :: !all_vulnerabilities
    end else
      print_endline "No vulnerabilities found."
    
  with
  | Failure msg -> Printf.printf "skipping file due to error: %s\n" msg
  | Yojson.Json_error msg -> Printf.printf "JSON Parser error: %s\n" msg
  | e -> Printf.printf "unexpected error: %s\n" (Printexc.to_string e)

let print_report () =
  print_endline "\n===================================================";
  print_endline "FINAL REPORT";
  print_endline "===================================================";
  
  if !all_vulnerabilities = [] then
    print_endline "No vulnerabilities were found in the scanned files."
  else begin
    Printf.printf "Total Vulnerable Files: %d\n\n" (List.length !all_vulnerabilities);
    
    List.iter (fun (file, vulns) ->
      Printf.printf "File: %s\n" file;
      List.iter (fun v ->
        Printf.printf "     Sink: %s\n" v.Analysis.sink_var;
        Printf.printf "     Message: %s\n" v.Analysis.message;
      ) vulns;
      print_newline ();
    ) !all_vulnerabilities
  end

let () =
  let target_dir = "../target-project/app/" in 
  print_endline "Starting Taint Analysis...";
  Printf.printf "Target Directory: %s\n" target_dir;

  let files = find_files target_dir in
  Printf.printf "Found %d files to analyze.\n" (List.length files);

  List.iter analyze_single_file files;

  print_report ();
  print_endline "Analysis Complete!"