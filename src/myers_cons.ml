(* A `cons`-list style implementation *)

type 'a cell = { next : 'a cell; len : int; value : 'a }

(**************************************)
(* High-performance functions *)

let init (_skip : int) (v : 'a) : 'a cell =
  let rec c : 'a cell = { next = c; len = 0; value = v } in
  c
[@@inline]

let cons (_skip : int) (v : 'a) (c : 'a cell) : 'a cell =
  { next = c; len = c.len + 1; value = v}
[@@inline]

let rec lookup (skip : int) (c : 'a cell) (l : int) : 'a cell =
  if c.len = l
  then c
  else lookup skip c.next l

let rec index (skip : int) (c : 'a cell) : int -> 'a cell = function
| 0 -> c
| i -> index skip c.next (i - 1)

let make_list_rev (skip : int) (v : 'a) (vs : 'a list) : 'a cell =
  let rec go (c : 'a cell) : 'a list -> 'a cell = function
  | [] -> c
  | v :: vs -> go (cons skip v c) vs in
  go (init skip v) vs

let make_tree_rev (skip : int) (height : int) (v : 'a) (vs : 'a list) : 'a cell =
  let rec go (last : 'a cell) (last_len : int) (vs : 'a list) : int -> 'a cell * 'a list = function
  | 0 -> let v' :: vs' = vs
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let c = { next = last; len = last_len + 1; value = v' } in
         (c, vs')
  | h -> let right, vs' = go last last_len vs (h - 1) in
         let left, v'' :: vs'' = go right (last_len + 1 lsl h - 1) vs' (h - 1)
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         ({ next = left; len = last_len + 2 lsl h - 1; value = v'' }, vs'') in
  let c, _ = go (init skip v) 0 vs (height - 1) in
  c

(**************************************)
(* Non-high-performance functions *)

let find_path (_skip : int) (c : 'a cell) (l : int) : 'a cell list =
  let rec go (c : 'a cell) (p : 'a cell list) : 'a cell list =
    if c.len = l
    then p
    else go c.next (c.next :: p) in
  go c [c]

let index_path (_skip : int) (c : 'a cell) (i : int) : 'a cell list =
  let rec go (c : 'a cell) (p : 'a cell list) : int -> 'a cell list = function
  | 0 -> p
  | i -> go c.next (c.next :: p) (i - 1) in
  go c [c] i

(**************************************)
(* Interface and generic functions *)
module Implementation : Myers.Implementation with type 'a t = 'a cell = struct
  type 'a t = 'a cell
  type 'a t' = 'a cell

  let next (c : 'a cell) : 'a cell = c.next
  let jump (c : 'a cell) : 'a cell = c.next
  let length (c : 'a cell) : int = c.len
  let car (c : 'a cell) : 'a = c.value
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
