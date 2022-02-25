let t =
  let doc = "integer to set for the answer type. Will be forced to 0 <= i <= 255 for the given integer." in
  Cmdliner.Arg.(value & pos 1 int 0 & info ~doc ~docv:"TYPE" [])

let answers =
  let doc = "lines to include in the answer. For best results,
    limit to four strings, of which the first is a maximum of 12 characters and the other three a maximum of 13." in
  Cmdliner.Arg.(value & pos_right 1 string [] & info ~doc ~docv:"ANSWERS" [])

let file =
  let doc = "file to write answers to. Will overwrite anything already there -- be cautious!" in
  Cmdliner.Arg.(value & pos 0 string "./Answers.dat" & info ~doc ~docv:"FILE" [])

let rec ncat n oc s =
  match n with
  | k when k <= 0 -> ()
  | k -> output_string oc s;
    ncat (k - 1) oc s

(* I know I don't need Faraday here. Shut up. You're not my mom. *)
(* Unless you're my mom. In which case, hi mom! <3 you're right
 * that I don't need Faraday. I love you, and I hope you
 * were also having a good time in the world while I was writing this!
 * Anyway neither of us did a good job of minimizing my library usage, but if
 * anybody else cares they can fork it. *)
let run file t answers =
  let oc = open_out file in
  let header_buf = Faraday.create 200 in
  let answer_buf = Faraday.create 52 in
  Answerer.Printer.header header_buf;
  Answerer.Printer.answer answer_buf t @@ List.map String.uppercase_ascii answers;
  let header = Faraday.serialize_to_string header_buf in
  output_string oc header;
  ncat 1000 oc @@ Faraday.serialize_to_string answer_buf;
  flush oc;
  exit 0

let () =
  let open Cmdliner.Term in
  exit @@ eval @@ ((const run $ file $ t $ answers), info "encoder")
