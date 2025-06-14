type 'a cell = { next : 'a cell; jump : 'a cell; length : int; value : 'a }
type 'a wrap = { head : 'a cell; last : 'a cell }

(**************************************)
(* High-performance functions *)

let init' (v : 'a) : 'a cell =
  let rec c : 'a cell = { next = c; jump = c; length = 0; value = v } in
  c
[@@inline]

let init (_skip : int) (v : 'a) : 'a wrap =
  let c : 'a cell = init' v in
  { head = c; last = c }
[@@inline]

let cons (_skip : int) (v : 'a) ({ head = h; last = l } : 'a wrap) : 'a wrap =
  let h_len = h.length in
  let h_hj = h_len - h.jump.length in
  if h_hj == 1 || h_hj == l.next.length - l.next.jump.length
  then let c = { next = h; jump = l.next; length = h_len + 1; value = v } in
       { head = c; last = l.jump }
  else let c = { next = h; jump = l; length = h_len + 1; value = v } in
       { head = c; last = c }
[@@inline]

let rec lookup (skip : int) (c : 'a cell) (l : int) : 'a cell =
  if c.length = l
  then c
  else let j = c.jump in
       let c' = if j.length < l then c.next else j in
       lookup skip c' l

let index (skip : int) (c : 'a cell) (i : int) : 'a cell = lookup skip c (c.length - i)
[@@inline]

(* Avoiding wrap until the top level improves the performance:
| make-list/3lgn/1_000_000 |     0.99 |  67.99ms | -2.29% +2.53% |  5.00Mw |   4.87Mw |   4.87Mw |     98.12% |
| make-list/2lgn/1_000_000 |     0.99 |  69.29ms | -2.16% +1.56% |  5.00Mw |   4.88Mw |   4.88Mw |    100.00% |
*)

let make_list_rev' (_skip : int) (v : 'a) (vs : 'a list) : 'a wrap =
  let rec go (h : 'a cell) (l : 'a cell) : 'a list -> 'a wrap = function
  | [] -> { head = h; last = l }
  | v :: vs ->
    let h_len = h.length in
    let h_hj = h_len - h.jump.length in
    if h_hj == 1 || h_hj == l.next.length - l.next.jump.length
    then let c = { next = h; jump = l.next; length = h_len + 1; value = v } in
         go c l.jump vs
    else let c = { next = h; jump = l; length = h_len + 1; value = v } in
         go c c vs in
  let last = init' v in
  go last last vs

(* Equivalent code but adds a wrap at each step

| make-list/3lgn/1_000_000 |     1.00 |  69.88ms | -1.70% +2.11% |  5.00Mw |   4.87Mw |   4.87Mw |     93.17% |
| make-list/2lgn/1_000_000 |     0.99 |  75.00ms | -1.60% +2.86% |  8.00Mw |   4.91Mw |   4.91Mw |    100.00% |

*)
let make_list_rev (skip : int) (v : 'a) (vs : 'a list) : 'a wrap =
  let rec go (b : 'a wrap) : 'a list -> 'a wrap = function
  | [] -> b
  | v :: vs -> go (cons skip v b) vs in
  go (init skip v) vs

let make_tree_rev (_skip : int) (height : int) (v : 'a) (vs : 'a list) : 'a wrap =
  let rec go (last : 'a cell) (last_len : int) (vs : 'a list) : int -> 'a cell * 'a list = function
  | 0 -> (last, vs)
  | h -> let right, v' :: vs' = go last last_len vs (h - 1)
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let left_last_len = last_len + 1 lsl h - 1 in
         let left_last = { next = right; jump = last; length = left_last_len; value = v' } in
         let left, v'' :: vs'' = go left_last left_last_len vs' (h - 1)
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let c = { next = left; jump = right; length = last_len + 2 lsl h - 2; value = v'' } in
         c, vs'' in
  let last = init' v in
  let c, _ = go last 0 vs (height - 1) in
  { head = c; last = last }

(**************************************)
(* Non-high-performance functions *)

let find_path (_skip : int) (c : 'a cell) (l : int) : 'a cell list =
  let rec go (c : 'a cell) (l : int) (p : 'a cell list) : 'a cell list =
    if c.length = l
    then p
    else let j = c.jump in
         let c' = if j.length < l then c.next else j in
         go c' l (c' :: p) in
  go c l [c]

let index_path (skip : int) (c : 'a cell) (i : int) : 'a cell list = find_path skip c (c.length - i)
[@@inline]

(**************************************)
(* Interface and generic functions *)
module Implementation : Myers.Implementation with type 'a t = 'a cell = struct
  type 'a t = 'a cell
  type 'a t' = 'a wrap

  let next (c : 'a cell) : 'a cell = c.next
  let jump (c : 'a cell) : 'a cell = c.jump
  let length (c : 'a cell) : int = c.length
  let car (c : 'a cell) : 'a = c.value
  let unwrap (b : 'a wrap) : 'a cell = b.head

  let init = init
  let cons = cons
  let lookup = lookup
  let index = index

  let make_list_rev = make_list_rev'
  let make_tree_rev = make_tree_rev

  let find_path = find_path
  let index_path = index_path
  let length_of_height _ h = 1 lsl h - 2
end

module Generic = Myers.Generic (Implementation)
