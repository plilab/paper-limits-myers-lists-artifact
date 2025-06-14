[@@@warning "-42"] (* Warning 42 = this use of structure relies on type-directed disambiguation,
it will not compile with OCaml 4.00 or earlier. *)

open Util

(* NOTE that in this file `depth` is really right-edge depth *)

(**************************************)

(* We also store the RHD and logical height in each cell for the convenience of calculation. *)
type 'a cell = {
  next : 'a cell;
  jump : 'a cell;
  length : int;
  ruler : int; (* The ruler value is the right-handed depth of a node. How far above a left-hand child is (0 = you are a left-hand child) *) (* Equivalent to reading off ruler sequence (starting from infinity) on each node at the same level *)
  height : int; (* Logical height of node *) (* NOTE: all heights in this file are logical *)
  value : 'a }

(* Normal: one cell *)
let normal_left c = c.next [@@inline] (* Left child *)
let normal_right c = c.jump [@@inline] (* Right child *)

(* Skip: two cells *)
let skip_left c = c.next.next [@@inline] (* Left child *)
let skip_right c = c.next.jump [@@inline] (* Right child *)
let skip_uncle c = c.jump [@@inline] (* Points to the uncle indexed by `c.ruler`, otherwise it points to the `c_0` *)

(* Leaf: three cells *)
let leaf_inc c = c.jump [@@inline] (* The next leaf whose ruler value is exactly one greater than the ruler value of the current leaf *)
let leaf_greater c = c.next.jump [@@inline] (* The next leaf whose ruler value is strictly greater than the ruler value of the current leaf *)
let leaf_uncle c = c.next.next.jump [@@inline] (* Points to the uncle indexed by `c.ruler`, otherwise it points to the `c_0` *)
let leaf_next c = c.next.next.next [@@inline] (* Points to the next node in the list *)

(* Path helpers *)
let cell_next_path f c p = let c' = c.next in f c' (c' :: p) [@@inline]
let cell_jump_path f c p = let c' = c.jump in f c' (c' :: p) [@@inline]

let skip_uncle_path f c p = cell_jump_path f c p [@@inline]

let leaf_inc_path f c p = cell_jump_path f c p [@@inline]
let leaf_greater_path f c p = cell_next_path (cell_jump_path f) c p [@@inline]
let leaf_uncle_path f c p = cell_next_path (cell_next_path (cell_jump_path f)) c p [@@inline]
let leaf_next_path f c p = cell_next_path (cell_next_path (cell_next_path f)) c p [@@inline]

(**************************************)
(* Tree sizes *)

let leafs_for_height (_skip : int) (height : int) = 1 lsl height
[@@inline]

let skips_for_height (skip : int) (height : int) =
  let skip_layers = height / skip in
  let skip_fanout = 1 lsl skip in
  let raw_skips = (pow skip_fanout skip_layers - 1) / (skip_fanout - 1) in
  let skips = (1 lsl (height mod skip)) * raw_skips in
  skips
[@@inline]

let normals_for_height (skip : int) (height : int) =
  1 lsl height - 1 - skips_for_height skip height
[@@inline]

let cells_for_height (skip : int) (height : int) = (* Cell count? count_cells number_of_cells *)
  3 * leafs_for_height skip height +
  2 * skips_for_height skip height +
  1 * normals_for_height skip height
[@@inline]

let length_of_height (skip : int) (height : int) = cells_for_height skip height - 1
[@@inline]

(* Returns the height of the effective root of a node with length in it, assuming it is at least "min_height" *)
let rec effective_root_height (skip : int) (length : int) (min_height : int) : int =
  if length < cells_for_height skip min_height
  then min_height
  else effective_root_height skip length (min_height + 1)

(* Returns the right-edge depth of the lowest node that is an ancestor of destination but not source; this is the uncle height *)
let uncle_effective_depth (skip : int) (source : int) (destination : int) : int  =
  let rec search_down (min_height : int) (source : int) (destination : int) = (*finds actual height of dif branch*)
    let size = cells_for_height skip min_height in
    if size > destination && size <= source
    then min_height (*end is under right branch, start is under left*)
    else if size > destination && size > source
    then search_down (min_height - 1) source destination (*both appear under right branch*)
    else search_down (min_height - 1) (source - size) (destination - size) (*Both appear under left.*) in
  let root_height = effective_root_height skip source 0 in (*Get root height. Only necessary to check start.*)
  root_height - search_down (root_height - 1) source destination (*Return the height where they differ, less the shared root.*)

(* Determines if destination is directly beneath source *)
let is_parent (skip: int) (c : 'a cell) (destination: int) : bool =
  c.length >= destination && destination > c.length - cells_for_height skip c.height
[@@inline]

(**************************************)
(* High-performance functions *)

let init (_skip : int) (v : 'a) : 'a cell =
  let rec c : 'a cell = { next = c; jump = c; length = 0; ruler = -1; height = -2; value = v } in
  c
[@@inline]

let cons (_skip : int) (_v : 'a) (_c : 'a cell) : 'a cell = failwith "unimplemented: Myers_1lgn.push"
[@@inline]

(* Find down through tree. Assumes destination is descendent of source *)
let rec find_child (destination : int) (c : 'a cell) : 'a cell =
  if c.length == destination
  then c
  else if c.jump.length < destination
  then find_child destination c.next
  else find_child destination c.jump

(* Find across leaves to a valid uncle. *)
let rec find_leaf (skip : int) (ruler: int) (destination : int) (c : 'a cell) : 'a cell =
  (* Use "uncle" *)
  if c.ruler == ruler
  then find_child destination (leaf_uncle c)

  (* Use "next" *)
  else if is_parent skip (leaf_next c) destination (*Note: Consider complexity*)
  then find_child destination (leaf_next c)

  (* Use "greater" *)
  else if is_parent skip (leaf_next (leaf_greater c)) destination (*Gross, but works*)
  then find_child destination (leaf_next (leaf_greater c))

  (* Use "inc" and thus keep going across fringe *)
  else find_leaf skip ruler destination (leaf_inc c)

(* Find down to a valid uncle. *)
let rec find_uncle (skip : int) (ruler : int) (destination : int) (c : 'a cell) : 'a cell =
  let find_branch (c : 'a cell) : 'a cell =
    (* Branch right or left depending on what RHD value we are looking for *)
    let delta = ruler - c.ruler in (* The amount by which we want to increase c.ruler *)
    if delta >= 0 && ( (* Go left if we need to decrease the RHD; this can only happen once. *)
        delta > c.height || (* If we're going to run out of tree, skip layers don't matter so go as far as possible.*)
        (c.height - delta) mod skip == 0) (* Following right branches will bring us to a skip that has the correct RHD value *)
    then find_uncle skip ruler destination c.jump
    else find_uncle skip ruler destination c.next in

  (* Normal node *)
  if c.height mod skip != 0
  then find_branch c

  (* Skip node *)
  else if c.height != 0
  then
    if c.ruler != ruler
    then find_branch c.next
    else find_child destination (skip_uncle c)

  (* Leaf node *)
  else find_leaf skip ruler destination c

(* Find starting from the start of a whole logical node *)
let rec find_whole (skip : int) (destination : int) (c : 'a cell) : 'a cell =
  if is_parent skip c destination then
    (* c is a parent of the destination, so use find_child *)
    find_child destination c
  else
    (* Since c is not a parent of the destination, try to find the uncle *)
    let ruler = uncle_effective_depth skip c.length destination - 1 in
    if c.height == 0 && c.ruler > ruler
    (* If we are at a leaf and c.ruler is already past ruler, then we should restart find_whole with the next structure *)
    (* This happens at most once per traversal *)
    then find_whole skip destination (leaf_next c)
    (* Otherwise, search for the uncle *)
    else find_uncle skip ruler destination c

(* Find starting from the middle of a leaf *)
let find_initial_leaf (skip : int) (destination : int) (c : 'a cell) : 'a cell =
  if c.jump.length != 0 && is_parent skip c.jump destination then
    (* c.jump points to our desired uncle *)
    find_child destination c.jump
  else
    (* start at the beginning of the next logical node *)
    find_whole skip destination c.next

(* Full lookup code. First finds the next cell which is at the root of its tree, then finds the next tree that is not a leaf node with ruler > 1, then paths *)
let lookup (skip : int) (c : 'a cell) (destination: int) : 'a cell =
  match c.length - destination with
  | 0 -> c
  | 1 -> c.next
  | _ ->
    match c.height with
    | h when h > 0 ->
      if h mod skip != 0 then
        (* Normal *)
        find_whole skip destination c
      else
        (* Skip *)
        if h == c.next.height
        then find_whole skip destination c
        else find_whole skip destination c.next
    (* Leaf cases *)
    |  0 -> find_whole skip destination c
    | -1 -> find_initial_leaf skip destination c.next
    | _ (*-2*) -> find_initial_leaf skip destination c

let index (skip : int) (c : 'a cell) (i : int) : 'a cell = lookup skip c (c.length - i)
[@@inline]

type 'a uncles_list = (int * 'a cell) list (* Always sorted in strictly decreasing order *)

let make_tree_rev (skip : int) (height : int) (v : 'a) (vs : 'a list) : 'a cell =

  let next_len : int ref = ref 0 in (* Next length after the ones used by the tree *)
  let vs : 'a list ref = ref (v :: vs) in (* Elements remaining to be added *)
  let leaf_incs : 'a cell option array = Array.make height None in (* build_tree_params.leaf_incs after leaves from the current tree are added *)
  let leaf_greats : 'a cell option array = Array.make height None in (* build_tree_params.leaf_greats after leaves from the current tree are added *)
  let c_0 : 'a cell ref = ref (Obj.magic 0) in (* The cell at length 0. Used as the cell to point two when no other cell is appropriate. *)

  let rec set_greats v n = if n >= 0 then (leaf_greats.(n) <- v; set_greats v (n - 1)) [@@inline] in

  let array_get_opt a r = if r < 0 then None else a.(r) [@@inline] in

  let make_leaf ~(ruler : int) ~(uncles : 'a uncles_list) ~(next : 'a cell) : 'a cell =
    let v1 :: v2 :: v3 :: vs' = !vs [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
    let inc_leaf : 'a cell option = array_get_opt leaf_incs ruler in
    let greater_leaf : 'a cell option = array_get_opt leaf_greats ruler in
    (* Since uncles is always in decreasing order, we use short-circuiting lookup *)
    let uncle : 'a cell option = assoc_decreasing_opt (ruler + 1) uncles in
    let length = !next_len in
    let rec bottom_cell = { (* This is the "Points to Uncle" node *) (* HOTSPOT *)
        (* Note the special case for the .len = 0 element.
           This is the only place bottom_cell is recursive. *)
        next = if length == 0 then bottom_cell else next;
        jump = if length == 0 then bottom_cell else or_else uncle !c_0;
        length = length;
        ruler = ruler;
        height = -2;
        value = v1 } in
    if length == 0 then c_0 := bottom_cell;
    let middle_cell = { (* This is the "Greater jump" node *) (* HOTSPOT *)
        next = bottom_cell;
        jump = or_else greater_leaf !c_0;
        length = length + 1;
        ruler = ruler;
        height = -1;
        value = v2 } in
    let top_cell = { (* This is the "Increment jump" node *) (* HOTSPOT *)
        next = middle_cell;
        jump = or_else inc_leaf !c_0;
        length = length + 2;
        ruler = ruler;
        height = 0;
        value = v3 } in
    next_len := length + 3;
    vs := vs';
    if ruler > 0 then
      (leaf_incs.(ruler - 1) <- Some top_cell;
       set_greats (Some top_cell) (ruler - 1));
    top_cell
    [@@inline] in

  let rec make_normal ~(depth : int) ~(height : int) ~(ruler : int) ~(uncles : 'a uncles_list) ~(next : 'a cell) : 'a cell =
    let right_result = make_tree
      ~depth:(match depth with | 0 -> 0 | n -> n + 1)
      ~height:(height - 1)
      ~ruler:(match ruler with | -1 -> -1 | n -> n + 1)
      ~uncles:uncles
      ~next:next in
    let left_result = make_tree
      ~depth:(depth + 1)
      ~height:(height - 1)
      ~ruler:0
      ~uncles:((depth + 1, right_result) :: uncles) (*Add right to uncles list for left subtree*) (* HOTSPOT *)
      ~next:right_result in
    let v :: vs' = !vs [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
    let branch : 'a cell = { (*Cell at root of branch node*) (* HOTSPOT *)
      next = left_result;
      jump = right_result;
      length = !next_len;
      ruler = ruler;
      height = height;
      value = v } in
    next_len := !next_len + 1;
    vs := vs';
    branch
    [@@inline]

  and make_skip ~(depth : int) ~(height : int) ~(ruler : int) ~(uncles : 'a uncles_list) ~(next : 'a cell) : 'a cell =
    (* A skip is just a node on top of a node that otherwise looks normal so we re-use `make_normal` *)
    let branch = make_normal ~depth ~height ~ruler ~uncles ~next in
    let uncle_tree : 'a cell option = List.assoc_opt (ruler + 1) uncles in
    let uncle_cell : 'a cell = or_else uncle_tree !c_0 in
    let v :: vs' = !vs [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
    let skip : 'a cell = { (*Skip structure's skip cell*) (* i.e., the head *)
      next = branch;
      jump = uncle_cell;
      length = !next_len;
      ruler = ruler;
      height = height;
      value = v } in
    next_len := !next_len + 1;
    vs := vs';
    skip
    [@@inline]

  and make_tree ~(depth : int) ~(height : int) ~(ruler : int) ~(uncles : 'a uncles_list) ~(next : 'a cell) : 'a cell =
    if height == 0 then
      make_leaf ~ruler ~uncles ~next
    else if height mod skip == 0 then
      make_skip ~depth ~height ~ruler ~uncles ~next
    else
      make_normal ~depth ~height ~ruler ~uncles ~next in

  if skip < 2 then failwith "ERROR in Myers_1lgn.make_tree_rev: skip less than two";
  make_tree
    ~depth:0
    ~height:height
    ~ruler:(-1)
    ~next:(Obj.magic 0)
    ~uncles:[]

let make_list_rev (skip : int) (v : 'a) (vs : 'a list) : 'a cell =
  if skip < 2 then failwith "ERROR in Myers_1lgn.make_tree_rev: skip less than two";
  let l = List.length vs in
  let h = effective_root_height skip l 0 in
  let vs' = List.rev_append (List.rev vs) (list_repeat [v] (length_of_height skip h)) in
  let c = make_tree_rev skip h v vs' in
  lookup skip c l

(**************************************)
(* Non-high-performance functions *)

(* Path down through tree. Assumes destination is descendent of source *)
let rec find_child_path (destination : int) (c : 'a cell) (path : 'a cell list) : 'a cell list =
  if c.length == destination
  then path
  else if c.jump.length < destination
  then cell_next_path (find_child_path destination) c path
  else cell_jump_path (find_child_path destination) c path

(* Path across leaves to a valid uncle. *)
let rec find_leaf_path (skip : int) (ruler: int) (destination : int) (c : 'a cell) (path: 'a cell list) : 'a cell list =
  (* Use "uncle" *)
  if c.ruler == ruler
  then leaf_uncle_path (find_child_path destination) c path

  (* Use "next" *)
  else if is_parent skip (leaf_next c) destination (*Note: Consider complexity*)
  then leaf_next_path (find_child_path destination) c path

  (* Use "greater" *)
  else if is_parent skip (leaf_next (leaf_greater c)) destination (*Gross, but works*)
  then leaf_greater_path (leaf_next_path (find_child_path destination)) c path

  (* Use "inc" and thus keep going across fringe *)
  else leaf_inc_path (find_leaf_path skip ruler destination) c path

(* Path down to a valid uncle. *)
let rec find_uncle_path (skip : int) (ruler : int) (destination : int) (c : 'a cell) (path : 'a cell list) : 'a cell list =
  let find_branch_path (c : 'a cell) (path: 'a cell list) : 'a cell list =
    (* Branch right or left depending on what ruler value we are looking for *)
    let delta = ruler - c.ruler in (* The amount by which we want to increase c.ruler *)
    if delta >= 0 && ( (* Go left if we need to decrease the ruler; this can only happen once. *)
        delta > c.height || (* If we're going to run out of tree, skip layers don't matter so go as far as possible.*)
        (c.height - delta) mod skip == 0) (* Following right branches will bring us to a skip that has the correct ruler value *)
    then cell_jump_path (find_uncle_path skip ruler destination) c path
    else cell_next_path (find_uncle_path skip ruler destination) c path in

  (* Normal node *)
  if c.height mod skip != 0
  then find_branch_path c path

  (* Skip node *)
  else if c.height != 0
  then
    if c.ruler != ruler
    then cell_next_path (find_branch_path) c path
    else skip_uncle_path (find_child_path destination) c path

  (* Leaf node *)
  else find_leaf_path skip ruler destination c path

(* Path starting from the start of a whole logical node *)
let rec find_whole_path (skip : int) (destination : int) (c : 'a cell) (path : 'a cell list) : 'a cell list =
  if is_parent skip c destination then
    (* c is a parent of the destination, so use find_child_path *)
    find_child_path destination c path
  else
    (* Since c is not a parent of the destination, try to find the uncle *)
    let ruler = uncle_effective_depth skip c.length destination - 1 in
    if c.height == 0 && c.ruler > ruler
    (* If we are at a leaf and c.ruler is already past RHD, then we should restart find_whole_path with the next structure *)
    (* This happens at most once per traversal *)
    then leaf_next_path (find_whole_path skip destination) c path
    (* Otherwise, search for the uncle *)
    else find_uncle_path skip ruler destination c path

(* Path starting from the middle of a leaf *)
let find_initial_leaf_path (skip : int) (destination : int) (c : 'a cell) (path : 'a cell list) : 'a cell list =
  if c.jump.length != 0 && is_parent skip c.jump destination then
    (* c.jump points to our desired uncle *)
    find_child_path destination c.jump (c.jump :: path)
  else
    (* start at the beginning of the next logical node *)
    find_whole_path skip destination c.next (c.next :: path)

(* Full path code. First finds the next cell which is at the root of its tree, then finds the next tree that is not a leaf node with ruler > 1, then paths *)
let find_path (skip : int) (c : 'a cell) (destination: int) : 'a cell list =
  match c.length - destination with
  | 0 -> [c]
  | 1 -> [c.next; c]
  | _ ->
    match c.height with
    | h when h > 0 ->
      if h mod skip != 0 then
        (* Normal *)
        find_whole_path skip destination c [c]
      else
        (* Skip *)
        if h == c.next.height
        then find_whole_path skip destination c [c]
        else find_whole_path skip destination c.next [c.next; c]
    (* Leaf cases *)
    |  0 -> find_whole_path skip destination c [c]
    | -1 -> find_initial_leaf_path skip destination c.next [c.next; c]
    | _ (*-2*) -> find_initial_leaf_path skip destination c [c]

let index_path (skip : int) (c : 'a cell) (i : int) : 'a cell list = find_path skip c (c.length - i)
[@@inline]

(**************************************)
(* Interface and generic functions *)
module Implementation : Myers.Implementation with type 'a t = 'a cell = struct
  type 'a t = 'a cell
  type 'a t' = 'a cell

  let next (c : 'a cell) : 'a cell = c.next
  let jump (c : 'a cell) : 'a cell = c.jump
  let length (c : 'a cell) : int = c.length
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
  let length_of_height = length_of_height
end

module Generic = Myers.Generic (Implementation)

(**************************************)
(* Non-interface functions *)

let tree_to_dot (skip : int) (graph_label : string) (highlight : 'a cell list)
      (show_distance : bool) (show_skip_uncle : bool) (show_leaf_uncle : bool)
      (show_inc : bool) (show_greater : bool) (c : 'a cell) : string =
  let b = Buffer.create 128 in
  let max_max_dist = ref 0 in
  let put (c : string) : unit = Buffer.add_string b c in
  let label (c : 'a cell) : string = match show_distance with
    | true  -> let d = array_max (Generic.distance false c) 0 in
               if d > !max_max_dist then max_max_dist := d;
               Printf.sprintf "%d\\nmax_dist:%d" c.length d (*a.(tree_root t).len)*)
    | false -> Printf.sprintf "%d" c.length in
  let id_cell (c : 'a cell) : string =
    Printf.sprintf "%d" (if c.length == 0 then 2 else c.length) in
  let put_edge' (s : string) (t1 : 'a cell) (t2 : string) : unit =
    put (Printf.sprintf "    %s -> %s %s;\n" (id_cell t1) t2 s) in
  let put_edge (s : string) (t1 : 'a cell) (t2 : 'a cell) : unit =
    put_edge' s t1 (id_cell t2) in
  let gen_id =
    let id_counter : int ref = ref 0 in
    fun () -> id_counter := !id_counter + 1; Printf.sprintf "%d" !id_counter in
  let layout_children (parent : 'a cell) (left : 'a cell) (right : 'a cell) =
    let center = "center" ^ gen_id () in
    put (Printf.sprintf "    %s [label=\" \",width=.1,height=.1,style=invis] ;\n" center);
    put_edge' "[style=invis]" parent center;
    put (Printf.sprintf "    {rank=same %s -> %s -> %s [style=invis]}\n" (id_cell left) center (id_cell right)) in
  let highlighting (cs : 'a cell list) : string =
    if List.exists (fun c -> List.memq c highlight) cs then "style=filled, fillcolor=\"#CCCCCC\"" else "" in
  let rec go (c : 'a cell) : unit =
    (* Normal *)
    if c.height > 0 && c.height mod skip != 0 then (
      put (Printf.sprintf "  %s [ shape=box, label=\"N:%s\\nruler:%d\", %s ];\n" (id_cell c) (label c) c.ruler (highlighting [c]));
      put_edge "[ style = dashed ]" c c.next;
      put_edge "" c c.jump;
      layout_children c c.next c.jump;
      go c.jump;
      go c.next
    (* Skip *)
    ) else if c.height > 0 then (
      put (Printf.sprintf "  %s [ shape=box, label=\"S:%s\\nruler:%d\", %s ];\n" (id_cell c) (label c) c.ruler (highlighting [c; c.next]));
      put_edge "[ style = dashed ]" c c.next.next;
      put_edge "" c c.next.jump;
      if show_skip_uncle && c.jump.length != 0 then put_edge "[ constraint = false, color=red ]" c c.jump;
      layout_children c c.next.next c.next.jump;
      go c.next.jump;
      go c.next.next
    (* Leaf *)
    ) else (
      put (Printf.sprintf "  %s [ shape=box, label=\"L:%s\\nruler:%d\", %s ];\n" (id_cell c) (label c) c.ruler (highlighting [c; c.next; c.next.next]));
      if show_inc && c.jump.length != 0 then (
        let i = c.jump in
        if i.length != 0 then
          let n = "inc" ^ (id_cell c) in
          put (Printf.sprintf "  %s [label=\"\", width=0,height=0, style=invis];\n" n);
          put (Printf.sprintf "  %s -> %s [color = green, arrowhead=none ];\n" (id_cell c) n);
          put (Printf.sprintf "  %s -> %s [dir=back, color = green ];\n" (id_cell i) n));
      if show_greater then (
        let i = c.next.jump in
        if i.length != 0 then
          let n = "greater" ^ (id_cell c) in
          put (Printf.sprintf "  %s [label=\"\", width=0,height=0, style=invis];\n" n);
          put (Printf.sprintf "  %s -> %s [color = blue, arrowhead=none ];\n" (id_cell c) n);
          put (Printf.sprintf "  %s -> %s [dir=back, color = blue ];\n" (id_cell i) n));
      put_edge "[ constraint = false, style=dashed ]" c c.next.next.next;
      let bottom = c.next.next in
      if show_leaf_uncle && bottom.jump.length != 0 then put_edge "[ constraint = false, color=red ]" c bottom.jump) in
  put "digraph g {\n";
(*  put "  ordering = out;\n";*)
  put (Printf.sprintf "  label=\"%s\"\n" graph_label);
(*  put "  rankdir = LR\n";*)
  go c;
  put "}\n";
  put (Printf.sprintf "// max max_dist: %d\n" !max_max_dist);
  Buffer.contents b
