module type Implementation = sig
  type 'a t (* Used when not constructing *)
  type 'a t' (* Used when creating, may be same as `t` *)

  (**************************************)
  (* Basic manipulation *)
  val next : 'a t -> 'a t (* Also known as cdr *)
  val jump : 'a t -> 'a t
  val length : 'a t -> int
  val car : 'a t -> 'a
  val unwrap : 'a t' -> 'a t

  (**************************************)
  (* Performance critical functions *)
  val init : (* skip : *) int -> 'a -> 'a t'
  val cons : (* skip : *) int -> 'a -> 'a t' -> 'a t'
  val lookup : (* skip : *) int -> 'a t -> (* len : *) int -> 'a t (* len is the length from the end to find *)
  val index : (* skip : *) int -> 'a t -> (* idx : *) int -> 'a t (* idx is the length from the front to find *)

  (* NOTE: take reversed list so they can be tail recursive *)
  (* first `int` is skip parameter *)
  val make_list_rev : (* skip : *) int -> 'a -> 'a list -> 'a t'
  val make_tree_rev : (* skip : *) int -> (* height : *) int -> 'a -> 'a list -> 'a t'

  (**************************************)
  (* Non-performance critical functions *)
  val find_path : (* skip : *) int -> 'a t -> (* len: *) int -> 'a t list
  val index_path : (* skip : *) int -> 'a t -> (* idx: *) int -> 'a t list
  val length_of_height : (* skip : *) int -> int -> int
end

module Generic (I : Implementation) = struct
  open Util
  open I

  (* Utility functions *)
  let make_list_ints (skip : int) : int -> int t' = function
  | 0 -> init skip 0
  | length -> make_list_rev skip 0 (1 -- length)

  let make_tree_ints (skip : int) : int -> int t' = function
  | 0 -> init skip 0
  | height -> make_tree_rev skip height 0 (1 -- I.length_of_height skip height)

  let to_list_list (c : 'a t) : 'a t list =
    let rec go (l : 'a t list) (c : 'a t) : 'a t list = match length c with
    | 0 -> c :: l
    | _ -> go (c :: l) (next c) in
    List.rev (go [] c)

  let to_list (c : 'a t) : 'a list =
    List.rev (List.rev_map car (to_list_list c))

  let equal (c1 : 'a t) (c2 : 'a t) : bool =
    let ht = Hashtbl.create 100 in
    let rec go : ('a t * 'a t) list -> bool = function
    | [] -> true
    | (c1, c2) :: cs ->
      match Hashtbl.find_opt ht c1 with
      | Some c2' -> c2 == c2' && go cs
      | None ->
         (length c1 == 0 && length c2 == 0) ||
         (Hashtbl.add ht c1 c2;
          go ((I.jump c1, I.jump c2) :: (I.next c1, I.next c2) :: cs)) in
    go [(c1, c2)]

  (* Tests *)

  (* tree matches list *)
  let test_tree_list (skip : int) (height : int) : bool =
    let tree_ints = I.unwrap (make_tree_ints skip height) in
    let list_ints = I.unwrap (make_list_ints skip (I.length tree_ints)) in
    equal tree_ints list_ints

  (* length_of_height matches make_tree_rev *)
  let test_length_of_height (skip : int) (height : int) : bool =
    let tree_ints = I.unwrap (make_tree_ints skip height) in
    I.length tree_ints == I.length_of_height skip height

  (* Test that find and index agree *)
  let test_find_index (skip : int) (length : int) : bool =
    let c = I.unwrap (make_list_ints skip length)
    and ok = ref true in
    for i = 0 to I.length c do
      let c1 = I.index skip c i
      and c2 = I.lookup skip c (length - i) in
      if c1 != c2
      then (ok := false;
            Printf.printf "FAILED: test_find_index len: %d skip: %d index: %d I.index.len: %d I.find.len: %d\n%!" length skip i (I.length c1) (I.length c2))
    done;
    !ok

  (* Test that find_path and find_index agree *)
  let test_find_path_index_path (skip : int) (length : int) : bool =
    let c = I.unwrap (make_list_ints skip length)
    and ok = ref true in
    for i = 0 to I.length c do
      let c1 = I.index_path skip c i
      and c2 = I.find_path skip c (length - i) in
      if List.length c1 != List.length c2 || not (List.for_all (fun b -> b) (List.map2 (fun c1 c2 -> c1 == c2) c1 c2))
      then (ok := false;
            Printf.printf "FAILED: test_find_path_index_path len: %d skip: %d index: %d I.index_path.len: %d I.find_path.len: %d\n%!" length skip i (List.length c1) (List.length c2))
    done;
    !ok

  (* Test that find matches find_path *)
  let test_find_find_path (skip : int) (length : int) : bool =
    let c = I.unwrap (make_list_ints skip length)
    and ok = ref true in
    for l = 0 to I.length c do
      let c1 = I.lookup skip c l
      and c2 = List.hd (I.find_path skip c l) in
      if c1 != c2
      then (ok := false;
            Printf.printf "FAILED: test_find_find_path len: %d skip: %d target_len: %d I.find.len: %d I.find_path.len: %d\n%!" length skip l (I.length c1) (I.length c2))
    done;
    !ok

  let rec check_path (last : int) : 'a t list -> bool = function
  | [] -> false
  | [x] -> I.length x == last
  | x1 :: x2 :: xs -> (x1 == I.next x2 || x1 == I.jump x2) && check_path last (x2 :: xs)

  (* Measure *)

  (* Returns an array containing the distance of each cell from the front *)
  (* The i'th element of that array corresponds to the cell with a len of i *)
  let distance (warn : bool) (c : 'a t) : int array =
    let a : int option array = Array.make (1 + length c) None in
    let rec go (i : int) (c : 'a t) : unit =
      let l = length c in
      let s = match a.(l) with
        | None -> true
        | Some i' ->
          if i < i'
          then ((if warn then Printf.printf "WARNING: smaller at %d: %d %d\n%!" l i i');
                true)
          else false in
      (if s then a.(l) <- Some i);
      (if s && l != 0
       then (go (i + 1) (jump c);
             go (i + 1) (next c))) in
    go 0 c;
    let f = function
    | None -> failwith "ERROR: missing entry in distance\n%!"
    | Some x -> x in
    Array.map f a

  (* Returns an array containing the cells that point to the given cell *)
  (* The i'th element of that array corresponds to the cell with a len of i *)
  let backpointers (c : 'a t) : 'a t list array =
    let r : 'a t list array = Array.make (1 + length c) [] in
    let rec init_r (c : 'a t) : unit =
      if length c != 0 then (
        r.(length (next c)) <- c :: r.(length (next c));
        if next c != jump c then
          r.(length (jump c)) <- c :: r.(length (jump c));
        init_r (next c)) in
    init_r c;
    r

  (* Returns a list of the cells that are in the optimal path from c to l *)
  (* Note that nodes from all optimal paths are included *)
  let find_opt_path (skip : int) (c : 'a t) (l :  int) : 'a t list =
    let d : int array = distance false c in
    let r : 'a t list array = backpointers c in
    let path : 'a t option array = Array.make (1 + length c) None in
    let rec go (dist : int) (c : 'a t) = match path.(length c) with
    | Some _ -> ()
    | None ->
       if d.(length c) == dist then (
         path.(length c) <- Some c;
         List.iter (go (dist - 1)) r.(length c)) in
    go d.(l) (lookup skip c l);
    List.map Util.option_get (List.filter Util.is_some (Array.to_list path))

  (*worst case (dynamic prog)*)
  (*worst case jump*)

  (* Benchmark *)
  (*make
  push
  find*)

  (* Display *)

  (*list*)

  (*path*)

  let to_dot (graph_label : string) (show_value : bool) (show_distance : bool) (c : 'a t) : string =
    let b = Buffer.create 128 in
    let put (c : string) : unit = Buffer.add_string b c in
    let a = if show_distance then distance false c else Array.make 0 0 in
    let label (c : 'a t) : string = match show_value, show_distance with
    | true , true  -> Printf.sprintf "%d [%d] = %d" (length c) a.(length c) (car c)
    | true , false -> Printf.sprintf "%d = %d" (length c) (car c)
    | false, true  -> Printf.sprintf "%d [%d]" (length c) a.(length c)
    | false, false -> Printf.sprintf "%d" (length c) in
    let show (c : 'a t) : unit =
      let n = if length c = 0 then c else next c in
      let j = if length c = 0 then c else jump c in
      put (Printf.sprintf "  %d [ label=\"%s\" ]; %d -> %d [ style = dashed ]; %d -> %d [contraint = false];\n"
             (length c) (label c)
             (length c) (length n)
             (length c) (length j)) in
    put "digraph g {\n";
    put (Printf.sprintf "  label=\"%s\"\n" graph_label);
    put "  rankdir = LR\n";
    List.iter show (List.rev (to_list_list c));
    put "}\n";
    Buffer.contents b

  (*
  dot (as list)
  dot (as tree)
  dot (with path)
  *)

end
