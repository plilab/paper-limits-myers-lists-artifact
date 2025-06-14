(* A list style implementation *)

type 'a cell = 'a list

(**************************************)
(* High-performance functions *)

let init (_skip : int) (v : 'a) : 'a cell = [v]
[@@inline]

let cons (_skip : int) (v : 'a) (c : 'a cell) : 'a cell = (List.cons[@inlined]) v c

let rec index (skip : int) ((_ :: c') as c : 'a cell) : int -> 'a cell = function
| 0 -> c
| i -> index skip c' (i - 1)
[@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *)

let lookup (skip : int) (c : 'a cell) (l : int) : 'a cell =
  index skip c (List.length c - l - 1)

let make_list_rev (_skip : int) (v : 'a) (vs : 'a list) : 'a cell = (List.rev[@inlined]) (v :: vs)

let make_tree_rev (skip : int) (height : int) (v : 'a) (vs : 'a list) : 'a cell =
  let rec go (last : 'a cell) (vs : 'a list) : int -> 'a cell * 'a list = function
  | 0 -> let v' :: vs' = vs
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let c = v' :: last in
         (c, vs')
  | h -> let right, vs' = go last vs (h - 1) in
         let left, v'' :: vs'' = go right vs' (h - 1)
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         (v'' :: left, vs'') in
  let c, _ = go (init skip v) vs (height - 1) in
  c

(**************************************)
(* Non-high-performance functions *)

let index_path (_skip : int) (c : 'a cell) (i : int) : 'a cell list =
  let rec go (c : 'a cell) (p : 'a cell list) : int -> 'a cell list = function
  | 0 -> p
  | i -> let (_ :: next) = c in go next (next :: p) (i - 1)
         [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
  go c [c] i

let find_path (skip : int) (c : 'a cell) (l : int) : 'a cell list =
  index_path skip c (List.length c - l - 1)

(**************************************)
(* Interface and generic functions *)
module Implementation : Myers.Implementation with type 'a t = 'a cell = struct
  type 'a t = 'a cell
  type 'a t' = 'a cell

  let next : 'a cell -> 'a cell = List.tl
  let jump : 'a cell -> 'a cell = List.tl
  let length (c : 'a cell) : int = List.length c - 1
  let car : 'a cell -> 'a = List.hd
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
