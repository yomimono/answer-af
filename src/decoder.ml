let file =
  let doc = "file to read answers from." in
  Cmdliner.Arg.(value & pos 0 string "./Answers.dat" & info ~doc [])

let run file =
  let ic = open_in_bin file in
  let l = in_channel_length ic in
  let b = Bytes.create l in
  let _l = input ic b 0 l in
  match Angstrom.parse_string ~consume:Prefix Answerer.Parser.file (Bytes.to_string b) with
  | Error s -> Format.eprintf "error parsing answers: %s\n%!" s; exit 1
  | Ok l ->
    List.iter (fun (c, l1, l2, l3, l4) ->
        Format.printf "%x: [%s] [%s] [%s] [%s]\n%!" c l1 l2 l3 l4
      ) l;
    exit 0


let () =
  let open Cmdliner.Term in
  exit @@ eval @@ ((const run $ file), info "decoder")
