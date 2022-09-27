let rec main_cmp ts =
  let cmp t1 t2 = print_endline (string_of_bool (Eval.compare t1 t2)) in
  match ts with
  | t1 :: t2 :: rest -> cmp t1 t2; main_cmp rest
  | _ :: [] -> print_endline "Skipping last term, nothing to compare against"
  | [] -> ()

let main_interpreter ts =
  let interpret (t, free_vars) = t |> Eval.normalize |> Printer.string_of_term free_vars |> print_endline in
  List.iter interpret ts

let main () =
  if Array.length Sys.argv < 2 then
    failwith ("Usage: " ^ Sys.argv.(0) ^ " [--cmp] <filename>");
  let cmp = Array.length Sys.argv = 3 && Sys.argv.(1) = "--cmp" in
  let filename = Sys.argv.(if cmp then 2 else 1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ts = Parser.main Lexer.main lexbuf in
  if cmp then
    main_cmp ts
  else
    main_interpreter ts

let () = main ()
