let t =
  let doc = "integer to set for the answer type. Will be forced to 0 <= i <= 255 for the given integer." in
  Cmdliner.Arg.(value & pos 1 int 0 & info ~doc [])

let answers =
  let doc = "lines to include in the answer. For best results,
    limit to four strings, of which the first is a maximum of 12 characters and the other three a maximum of 13." in
  Cmdliner.Arg.(value & pos_right 1 string [] & info ~doc [])

let file =
  let doc = "file to write answers to. Will overwrite anything already there -- be cautious!" in
  Cmdliner.Arg.(value & pos 0 string "./Answers.dat" & info ~doc [])

let rec ncat n oc s =
  match n with
  | k when k <= 0 -> ()
  | k -> output_string oc s;
    ncat (k - 1) oc s

let run file t answers =
  let oc = open_out file in
  let header_buf = Faraday.create 200 in
  let answer_buf = Faraday.create 52 in
  Answerer.header header_buf;
  Answerer.answer answer_buf t @@ List.map String.uppercase_ascii answers;
  output_string oc (Faraday.serialize_to_string header_buf);
  ncat 1000 oc @@ Faraday.serialize_to_string answer_buf;
  flush oc;
  exit 0

let () =
  let open Cmdliner.Term in
  exit @@ eval @@ ((const run $ file $ t $ answers), info "answerer")
