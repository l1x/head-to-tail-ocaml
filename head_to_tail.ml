open Re2

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
  remove [word;] (distinct (List.filter ~f:(fun w ->  word_matcher w pattern) word_list))

let is_empty (tup : (bytes * bytes list)) =
  match tup with 
  | (_,[]) -> false 
  | (_,_) -> true 

let rec build_adj_list_aux dict acc =
  match dict with
  | [] -> acc
  | h::t -> 
    let wl = (find_words h (remove [h] t)) in
    build_adj_list_aux (remove wl t) ((h, wl) :: acc)

let build_adj_list ~word:(word : bytes) : (bytes * bytes list) list =
  let dict = read_file_lines "wordsEn.txt" in
  let length = String.length word in
  let nword_dict = List.filter ~f:(fun word_aux -> same_lenght length word_aux) dict in
  List.filter ~f:(is_empty) (build_adj_list_aux nword_dict [])

let list_to_string (l : bytes list) : bytes = 
  Bytes.cat (Bytes.concat " : " l ) "\n"

let rec print_nested (l : (bytes * bytes list) list) = 
  match l with
  | [] -> print_bytes "ok \n"
  | (w, wl)::t -> print_bytes (w ^ " :: " ^ (list_to_string wl));print_nested t

let main () =
  print_nested (build_adj_list ~word:"ape")

let _ = main ()