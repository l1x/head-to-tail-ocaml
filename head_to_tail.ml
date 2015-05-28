open Core
open Core_extended
open Re2

(*module Re2 = Re2.Std.Re2*)
module List = Core.Std.List
module Result = Core.Std.Result
module String = Core.Std.String
module In_channel = Core.Std.In_channel
module Array = Core_extended.Std.Array

let range ~small:a ~big:b =
  let rec range_aux ~small:a ~big:b ~accumulator:acc =
    if a > b then acc
    else range_aux ~small:(a+1) ~big:b ~accumulator:(a :: acc) in
  List.rev (range_aux ~small:a ~big:b ~accumulator:[])

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

let list_to_string (l : bytes list) : bytes = 
  Bytes.cat (Bytes.concat " : " l ) "\n"

let matrix n m init =
    let result = Array.create n (Array.create m init) in
    for i = 1 to n - 1 do
      result.(i) <- Array.create m init
    done;
    result

let half_matrix_coordinates n =
  (*
  returns the coordinates of the valid points above any given n
  1 -> 1,0
  2 -> 2,0 2,1 
  ...
  *)
  List.map ~f:(fun x -> (n, x) ) (range 0 (n -1))

let half_matrix n =
  (*returns a complete half matrix of n elements*)
  List.map ~f:(fun x -> half_matrix_coordinates x) (range 1 n) 

let is_match (tup) (arr) =
  let (a,b) = tup in
  let a_word = arr.(a) in
  let b_word = arr.(b) in
  let pattern = Bytes.concat "|" (one_dot_words ~string:a_word ~char:'.') in
  Re2.matches (Re2.create_exn pattern) b_word

let word_matcher (word : bytes) (pattern : bytes) =
  Result.is_ok (Re2.find_first (Re2.create_exn pattern) word)

let build_adj_list ~word:(word : bytes) : (int * int) list=
  let dict = read_file_lines "wordsEn.txt" in
  let length = String.length word in
  let nword_dict = Array.of_list (List.filter ~f:(fun word_aux -> same_lenght length word_aux) dict) in
  let num_elements = (Array.length nword_dict) - 1 in
  let adj_list = List.concat (half_matrix num_elements) in
  print_bytes ("# of items: " ^ (string_of_int (List.length adj_list)));
  List.filter ~f:(fun x -> is_match x nword_dict) adj_list

let tup_to_string (tup : (int * int)) =
  let (a,b) = tup in
  "(" ^ (string_of_int a) ^ ":" ^ (string_of_int b) ^ ")"

let main () =
  let adj_list = (build_adj_list ~word:"ape") in
  (*print_bytes (Bytes.concat ", " (List.map ~f:(tup_to_string) adj_list));*)
  print_endline "";
  print_endline (string_of_int (List.length adj_list))

let _ = main ()