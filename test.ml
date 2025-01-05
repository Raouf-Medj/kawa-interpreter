open Sys
open Printf

(* Function to color the text green for OK and red for FAIL *)
let green text = sprintf "\027[32m%s\027[0m" text
let red text = sprintf "\027[31m%s\027[0m" text

(* Function to run the kawai.exe program with a given file *)
let run_kawai file =
  let command = sprintf "./kawai.exe %s" file in
  try
    let ic = Unix.open_process_in command in
    let result = input_line ic in
    let _ = Unix.close_process_in ic in
    Some result
  with
  | _ -> None

(* Function to run all tests in the ./tests/ directory *)
let run_tests () =
  let test_dir = "./tests/" in
  if not (Sys.file_exists test_dir) || not (Sys.is_directory test_dir) then
    printf "Directory %s does not exist or is not a directory.\n" test_dir
  else
    let files = Array.to_list (Sys.readdir test_dir) in
    let kwa_files = List.filter (fun f -> Filename.check_suffix f ".kwa") files in
    List.iter
      (fun file ->
        let filepath = Filename.concat test_dir file in
        printf "Testing %s... " file;
        match run_kawai filepath with
        | Some result -> printf "%s (%s)\n" (green "OK") result
        | None -> printf "%s (Execution error)\n" (red "FAIL"))
      kwa_files

let () =
  run_tests ()
