(* A list style implementation *)

type 'a cell = (int * 'a) list

(**************************************)
(* High-performance functions *)

let init (_skip : int) (v : 'a) : 'a cell = [(0, v)]
[@@inline]

let cons (_skip : int) (v : 'a) (((i, _) :: _) as c : 'a cell) : 'a cell = (i + 1, v) :: c
[@@inline]
[@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *)

let rec lookup (skip : int) (((i, _) :: c') as c : 'a cell) (l : int) : 'a cell =
  if i = l
  then c
  else lookup skip c' l
[@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *)

let rec index (skip : int) ((_ :: c') as c : 'a cell) : int -> 'a cell = function
| 0 -> c
| i -> index skip c' (i - 1)
[@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *)

let make_list_rev (_skip : int) (v : 'a) (vs : 'a list) : 'a cell =
  let rec go (i : int) (c : 'a cell) : 'a list -> 'a cell = function
  | [] -> c
  | v :: vs -> go (i + 1) ((i, v) :: c) vs in
  go 1 [(0, v)] vs

let make_tree_rev (skip : int) (height : int) (v : 'a) (vs : 'a list) : 'a cell =
  let rec go (last : 'a cell) (last_len : int) (vs : 'a list) : int -> 'a cell * 'a list = function
  | 0 -> let v' :: vs' = vs
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let c = (last_len + 1, v') :: last in
         (c, vs')
  | h -> let right, vs' = go last last_len vs (h - 1) in
         let left, v'' :: vs'' = go right (last_len + 1 lsl h - 1) vs' (h - 1)
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         ((last_len + 2 lsl h - 1, v'') :: left, vs'') in
  let c, _ = go (init skip v) 0 vs (height - 1) in
  c

(**************************************)
(* Non-high-performance functions *)

let find_path (_skip : int) (c : 'a cell) (l : int) : 'a cell list =
  let rec go (((i, _) :: next) : 'a cell) (p : 'a cell list) : 'a cell list =
    if i = l
    then p
    else go next (next :: p)
  [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
  go c [c]

let index_path (_skip : int) (c : 'a cell) (i : int) : 'a cell list =
  let rec go ((_ :: next) : 'a cell) (p : 'a cell list) : int -> 'a cell list = function
  | 0 -> p
  | i -> go next (next :: p) (i - 1)
  [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
  go c [c] i

(**************************************)
(* Interface and generic functions *)
module Implementation : Myers.Implementation with type 'a t = 'a cell = struct
  type 'a t = 'a cell
  type 'a t' = 'a cell

  let next : 'a cell -> 'a cell = List.tl
  let jump : 'a cell -> 'a cell = List.tl
  let length (c : 'a cell) : int = fst (List.hd c)
  let car (c : 'a cell) : 'a = snd (List.hd c)
  let unwrap (c : 'a cell) : 'a cell = c

  let init = init
  let cons = cons
  let lookup = lookup
  let index = index

  let make_list_rev = make_list_rev
  let make_tree_rev = make_tree_rev

  let find_path = find_path
  let index_path = index_path
  let length_of_height _ h = 1 lsl h - 1
end

module Generic = Myers.Generic (Implementation)
