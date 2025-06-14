(* An empty implementation for testing benchmark overhead *)

type 'a cell = 'a

(**************************************)
(* High-performance functions *)

let init  (_skip : int) (v : 'a) : 'a cell = v [@@inline]
let cons  (_skip : int) (_v : 'a) (c : 'a cell) : 'a cell = c
let lookup  (_skip : int) (c : 'a cell) (_len : int) : 'a cell = c
let index (_skip : int) (c : 'a cell) (_idx : int) : 'a cell = c
let make_list_rev (_skip : int) (v : 'a) (_vs : 'a list) : 'a cell = v
let make_tree_rev (_skip : int) (_height : int) (v : 'a) (_vs : 'a list) : 'a cell = v

(**************************************)
(* Non-high-performance functions *)

let find_path  (_skip : int) (c : 'a cell) (_len : int) : 'a cell list = [c]
let index_path (_skip : int) (c : 'a cell) (_idx : int) : 'a cell list = [c]

(**************************************)
(* Interface and generic functions *)
module Implementation : Myers.Implementation with type 'a t = 'a cell = struct
  type 'a t = 'a cell
  type 'a t' = 'a cell

  let next (c : 'a cell) : 'a cell = c
  let jump (c : 'a cell) : 'a cell = c
  let length (_c : 'a cell) : int = 0
  let car (c : 'a cell) : 'a = c
  let unwrap (c : 'a cell) : 'a cell = c

  let init = init
  let cons = cons
  let lookup = lookup
  let index = index

  let make_list_rev = make_list_rev
  let make_tree_rev = make_tree_rev

  let find_path = find_path
  let index_path = index_path
  let length_of_height _ _ = 0
end

module Generic = Myers.Generic (Implementation)
