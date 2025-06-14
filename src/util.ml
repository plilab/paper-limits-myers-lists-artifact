(**************************************)
(* Ints *)

let rec pow (base : int) : int -> int = function
| 0 -> 1
| n -> base * pow base (n - 1)

(**************************************)
(* Lists *)

(* `i -- j` returns a list of integer between i and j inclusive *)
(* If i > j, then the list counts down.  Otherwise, it counts up. *)
let (--) (i : int) (j : int) : int list =
  let rec go (j : int) (l : int list) =
    if i < j then go (j - 1) (j :: l)
    else if i > j then go (j + 1) (j :: l)
    else l in
  i :: go j []

let to_by (i : int) (step : int) (j : int) : int list =
  let rec go (j : int) (l : int list) =
    if j < i then l
    else go (j - step) (j :: l) in
  let last = ((j - i) / step) * step + i in
  go last []

let list_repeat (l : 'a list) (n : int) : 'a list =
  let l' = List.rev l in
  let rec go (x : 'a list) : int -> 'a list = function
  | 0 -> x
  | n -> go (List.rev_append l' x) (n - 1) in
  go [] n

let rec assoc_ge_opt (k : int) : (int * 'a) list -> 'a option = function
| [] -> None
| (i, t) :: rest -> if i >= k then Some t else assoc_ge_opt k rest

let rec assoc_decreasing_opt (k : int) : (int * 'a) list -> 'a option = function
| [] -> None
| (i, t) :: rest -> if k == i then Some t else if k > i then None else assoc_decreasing_opt k rest

(**************************************)
(* Arrays *)

let array_arg_max (a : int array) (max : int * int) : int * int =
  let rec go (max : int * int) : int -> int * int = function
  | 0 -> max
  | i -> go (if a.(i - 1) > snd max then (i - 1, a.(i - 1)) else max) (i - 1) in
  go max (Array.length a)

let array_arg_maxs (a : int array) (max : int) : int list * int =
  let rec go (max : int list * int) : int -> int list * int = function
  | 0 -> max
  | i -> let max' = if a.(i - 1) > snd max then ([i - 1], a.(i - 1))
                    else if a.(i - 1) == snd max then (i - 1 :: fst max, snd max)
                    else max in
         go max' (i - 1) in
  go ([], max) (Array.length a)

let array_max (a : int array) (max : int) : int =
  snd (array_arg_max a (max, 0))

let array_sum (a : int array) : int = Array.fold_left (+) 0 a

let array_avg (a : int array) : float = float_of_int (array_sum a) /. float_of_int (Array.length a)

let array_distribution (a : int array) : int array =
  let b : int array = Array.make (1 + array_max a 0) 0 in
  Array.iter (fun x -> b.(x) <- 1 + b.(x)) a;
  b

(**************************************)
(* Shuffling *)

let swap (a : 'a array) (i : int) (j : int) : unit =
  let x = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- x

let array_shuffle (a : 'a array) : unit =
  List.iter (fun i -> swap a i (Random.int (i + 1))) ((Array.length a - 1) -- 1)

let list_shuffle (l : 'a list) : 'a list =
  let a = Array.of_list l in
  array_shuffle a;
  Array.to_list a

(**************************************)
(* Options *)

let or_else (a : 'a option) (b : 'a) : 'a = match a with
| Some x -> x
| None -> b

let is_some : 'a option -> bool = function
| Some _ -> true
| None -> false

let option_get : 'a option -> 'a = function
| Some x -> x
| None -> failwith "called `get_option` on None"

let option_map (f : 'a -> 'b) : 'a option -> 'b option = function
| Some x -> Some (f x)
| None -> None

(**************************************)
(* Strings *)

let ints_of_string (s : string) : int list =
  let f (s : string) : int list =
    match String.split_on_char '-' s with
    | [s] -> [int_of_string s]
    | [s1; s2] -> int_of_string s1 -- int_of_string s2
    | [s1; s2; s3] -> to_by (int_of_string s1) (int_of_string s2) (int_of_string s3)
    | _ -> failwith (Printf.sprintf "Invalid number of dashes in '%s'" s) in
  List.concat (List.map f (String.split_on_char ',' s))

let ints_of_strings (ss : string list) : int list =
  List.concat (List.map ints_of_string ss)

let two_ints_of_string (s : string) : int * int = match String.split_on_char ',' s with
| [s1; s2] -> int_of_string s1, int_of_string s2
| _ -> failwith (Printf.sprintf "Invalid number of commas in '%s" s)

(* `indent i` returns a string of i spaces *)
let rec indent : int -> string = function
| 0 -> ""
| n -> " " ^ indent (n -1)

(* Helpers for converting common values to strings *)
let format_int (i : int) : string = Printf.sprintf "%d" i

let format_option (f : 'a -> string) (x : 'a option) : string = match x with
| None -> "None"
| Some x -> "(Some " ^ f x ^ ")"

let rec format_list (f : 'a -> string) (x : 'a list) : string = match x with
| [] -> "[]"
| x :: xs -> "(" ^ f x ^ " :: " ^ format_list f xs ^ ")"

let format_array (f : 'a -> string) (x : 'a array) : string =
  Printf.sprintf "[| %s |]" (String.concat "; " (Array.to_list (Array.map f x)))

let format_pair (f : 'a -> string) (g : 'b -> string) (x : 'a * 'b) : string = match x with
| (x, y) -> "(" ^ f x ^ ", " ^ g y ^ ")"

let format_bool (x : bool) : string = match x with
| true -> "true"
| false -> "false"

let format_string (x : string) : string = "\"" ^ x ^ "\""
