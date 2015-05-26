module Re2 = Re2.Std.Re2
module List = Core.Std.List
module Result = Core.Std.Result
module String = Core.Std.String
module In_channel = Core.Std.In_channel

let read_file_lines (input_file : string) : string list =
  In_channel.read_lines input_file

let rec distinct x =
  let rec distinct_help l n =
    match l with
    | [] -> []
    | h :: t -> if n = h then distinct_help t n else h::(distinct_help t n) in
     match x with
     | [] -> []
     | h::t -> h::(distinct_help (distinct t) h)

let rec remove xs ys=
  match xs, ys with
  | [], _ -> ys
  | h1::t1, ys -> remove t1 (List.filter ~f:(fun w -> w <> h1) ys)

let same_lenght (length : int ) (word : string ) : bool =
  if (String.length word) = length then true else false

let count_lines (input_file : string) : int =
  let file_lines = read_file_lines input_file in
  List.count ~f:(fun _ -> true) file_lines
 
let rec one_dot_words_aux (str : bytes) (chr : char) (pos : int) (acc : bytes list) : bytes list =
  (* OCaml string are mutable :( ) *)
  let max = Bytes.length str in
  let tmp = Bytes.copy str in
  if pos >= max then 
    acc 
  else
    one_dot_words_aux str chr (pos + 1) ((Bytes.set tmp pos chr;tmp) :: acc )

let one_dot_words ~string:(str : bytes) ~char:(chr : char) =
  one_dot_words_aux str chr 0 []

let word_matcher (word : bytes) (pattern : bytes) =
  Result.is_ok (Re2.find_first (Re2.create_exn pattern) word)

let find_words (word : bytes) (word_list : bytes list) =
  let pattern = Bytes.concat "|" (one_dot_words ~string:word ~char:'.') in
  remove [word;] (distinct (List.filter ~f:(fun word_lambda ->  word_matcher word_lambda pattern) word_list))

let rec build_adj_list_aux (word : bytes) (dict) (acc) =
  let word_list = remove word (distinct (find_words word dict)) in
  match dict with
   | [] -> acc
   | _ -> build_adj_list_aux word (remove word_list dict) ((word, word_list) :: acc)

let build_adj_list ~word:(word : bytes) : (bytes * bytes list) list =
  let dict = read_file_lines "wordsEn.txt" in
  let length = String.length word in
  let nword_dict = List.filter ~f:(fun word_aux -> same_lenght length word_aux) dict in
  build_adj_list_aux word nword_dict []

(* ((word, list) (word, list) (word,list)...)

(bytes * bytes list) list =                                                                                
[ ("aah", ["aah"; "ash"; "bah"; "hah"; "rah"]); 
  ("abc", ["abc"; "abo"; "abs"; "abt"; "arc"]);                    
  ("abo", ["abc"; "abo"; "abs"; "abt"; "ado"; "ago"]);
  ("abs", ["abc"; "abo"; "abs"; "abt"; "ads"; "ahs"; "ars"; "ass"; "lbs"; "tbs"]);
  ("abt", ["abc"; "abo"; "abs"; "abt"; "act"; "aft"; "alt"; "ant"; "apt"; "art"]);
  ("ace", ["ace"; "act"; "age"; "ale"; "ape"; "are"; "ate"; "ave"; "awe"; "axe"; "aye"; "ice"]);
  ("act", ["abt"; "ace"; "act"; "aft"; "alt"; "ant"; "apt"; "art"; "jct"; "pct"]);
  ("add", ["add"; "adj"; "ado"; "ads"; "adv"; "adz"; "aid"; "and"; "aud"; "odd"]);
*)
  
  (*
 List.(range 0 n >>| fun x -> x * 2 + 1);

List.map ~f:(fun x -> (x, Bytes.uppercase x)) ["aa";"bb";];;
    [[][]] -> {:word ["word"];}*)
(*
let testing  () =
  [ Bench.Test.create ~name:"read_file_lines"
      (fun () -> read_file_lines ("wordsEn.txt"));
    Bench.Test.create ~name:"count_lines"
      (fun () -> count_lines ("wordsEn.txt"));
  ]
  |> Bench.make_command
  |> Command.run
*)

let list_to_string (l : bytes list) : bytes = 
  Bytes.cat (Bytes.concat " : " l ) "\n"

let rec print_nested (l : (bytes * bytes list) list) = 
  match l with
  | [] -> print_bytes "ok \n"
  | (h::t)::t2 -> print_bytes h;print_nested t

let 

let main () =
  print_nested (build_adj_list ~word:"ape")


  (**)

(*
  testing ()
  let counter = count_lines "wordsEn.txt" in
  let outp = string_of_int counter in 
  print_endline "szop"
*)

let _ = main ()