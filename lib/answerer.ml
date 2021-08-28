let bytewise_xor key s =
  String.map (fun c -> (int_of_char c) lxor key |> Char.chr) s

let s1_key = 0x4a
let s2_key = 0x61
let s3_key = 0x6b
let s4_key = 0x65

let header b =
  Faraday.write_uint8 b 0xf0;
  Faraday.write_uint8 b 0x01;
  for _ = 0 to 197 do
  Faraday.write_uint8 b 0
done


let literally_answer b first_byte s1 s2 s3 s4 =
  let open Faraday in
  write_uint8 b first_byte;
  write_string b ~len:12 (bytewise_xor s1_key s1);
  write_string b ~len:13 (bytewise_xor s2_key s2);
  write_string b ~len:13 (bytewise_xor s3_key s3);
  write_string b ~len:13 (bytewise_xor s4_key s4);
  ()

let spaces_string n = String.init n (fun _ -> ' ')

let answer b first_byte l =
  let f = literally_answer b first_byte in
  match l with
  | a :: b :: c :: d :: _ -> f a b c d
  | a :: b :: c :: [] -> f a b c (spaces_string 13)
  | a :: b :: [] -> f (spaces_string 12) a b (spaces_string 13)
  | a :: [] -> f (spaces_string 12) a (spaces_string 13) (spaces_string 13)
  | [] -> f (spaces_string 12) (spaces_string 13) (spaces_string 13) (spaces_string 13)
