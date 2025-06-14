(* This file should contain only top-level driver code *)

(* Library modules *)
module Command = Core.Command (* We avoid importing `Core` as that shadows things *)
open Core_bench
open Command.Let_syntax (* used by let%map_open *)

(* Local modules *)
open Benchmarking
open Util

(* Implementation names *)

let impl_of_string : string -> string * (module Myers.Implementation) = function
| "noop" -> "noop", (module Myers_noop.Implementation)
| "list" -> "list", (module Myers_list.Implementation)
| "pair" -> "pair", (module Myers_pair.Implementation)
| "cons" -> "cons", (module Myers_cons.Implementation)
| "3lgn" -> "3lgn", (module Myers_3lgn.Implementation)
| "2lgn" -> "2lgn", (module Myers_2lgn.Implementation)
| "1lgn" -> "1lgn", (module Myers_1lgn.Implementation)
| impl -> failwith (Printf.sprintf "unknown implementation name: %s" impl)

let impls_of_string (s : string) : (string * (module Myers.Implementation)) list =
  let rec f : string -> (string * (module Myers.Implementation)) list = function
    | "all"  -> List.append (f "most") [
                 impl_of_string "1lgn"]
    | "most" -> [impl_of_string "noop";
                 impl_of_string "list";
                 impl_of_string "pair";
                 impl_of_string "cons";
                 impl_of_string "3lgn";
                 impl_of_string "2lgn"]
    | i -> [impl_of_string i] in
  List.concat (List.map f (String.split_on_char ',' s))

let impls_of_strings (ss : string list) : (string * (module Myers.Implementation)) list =
  List.concat (List.map impls_of_string ss)

(* Benchmarking *)

let fix_list (l : ('a * 'a list)) : 'a list =
  let (hd, tl) = l in
  hd :: tl

let bench_make_list_command : string * Command.t =
  ("make-list",
    Bench.make_command_ext ~summary:"benchmark `make_list_rev`"
      (let%map_open length = flag "length" (one_or_more string) ~doc:"{LENGTHS} length"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name" in
       make_bench_command
         (List.concat
           (List.map (fun l ->
             let vs = 1 -- l in
             List.map (fun (i, m) ->
               let module I = (val (m : (module Myers.Implementation))) in
               Bench.Test.create ~name:(Printf.sprintf "make-list/impl:%s/len:%d" i l)
                 (fun () -> ignore (I.make_list_rev skip 0 vs)))
               (impls_of_strings (fix_list impl)))
             (ints_of_strings (fix_list length))))))

let bench_make_tree_command : string * Command.t =
  ("make-tree",
    Bench.make_command_ext ~summary:"benchmark `make_tree_rev`"
      (let%map_open height = flag "height" (one_or_more string) ~doc:"{HEIGHTS} height"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name" in
       make_bench_command
         (List.concat
           (List.map (fun h ->
             List.map (fun (i, m) ->
               let module I = (val (m : (module Myers.Implementation))) in
               let l = I.length_of_height skip h in
               let vs = 1 -- l in
               Bench.Test.create ~name:(Printf.sprintf "make-tree/impl:%s/height:%d/len:%d" i h l)
                 (fun () -> ignore (I.make_tree_rev skip h 0 vs)))
               (impls_of_strings (fix_list impl)))
             (ints_of_strings (fix_list height))))))

let bench_index_command : string * Command.t =
  ("index",
    Bench.make_command_ext ~summary:"benchmark `index`"
      (let%map_open length = flag "length" (one_or_more string) ~doc:"{LENGTHS} length"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name"
                and index = flag "index" (one_or_more string) ~doc:"{INDEXES} the indexes to lookup"
                and shuffle = flag "shuffle" (no_arg) ~doc:"shuffle the order of indexes" in
       make_bench_command
         (List.concat
           (List.map (fun l ->
             let vs = 1 -- l in
             let is0 = ints_of_strings (fix_list index) in
             let is = if shuffle then list_shuffle is0 else is0 in
             List.map (fun (i, m) ->
               let module I = (val (m : (module Myers.Implementation))) in
               let s = I.unwrap (I.make_list_rev skip 0 vs) in
               Bench.Test.create ~name:(Printf.sprintf "index/impl:%s/len:%d" i l)
                 (fun () -> ignore (List.iter (fun i -> ignore (I.index skip s i)) is)))
               (impls_of_strings (fix_list impl)))
             (ints_of_strings (fix_list length))))))

let bench_worst_command : string * Command.t =
  ("worst",
    Bench.make_command_ext ~summary:"benchmark worst-case for `index`"
      (let%map_open length = flag "length" (one_or_more string) ~doc:"{LENGTHS} length"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name"
                and repeat = flag "repeat" (optional_with_default 1 int) ~doc:"COUNT how many times to repeat worst case"
                and shuffle = flag "shuffle" (no_arg) ~doc:"shuffle the order of indexes"
                and reverse = flag "reverse" (no_arg) ~doc:"reverse the order of indexes" in
       make_bench_command
         (List.concat
           (List.map (fun l ->
             let vs = 1 -- l in
             List.map (fun (i, m) ->
               let module I = (val (m : (module Myers.Implementation))) in
               let module G = Myers.Generic (I) in
               let s = I.unwrap (I.make_list_rev skip 0 vs) in
               let (ind, d) = array_arg_maxs (G.distance false s) (-1) in
               let ind1 = list_repeat (List.map (fun i -> l - i) ind) repeat in
               let ind2 = if reverse then List.rev ind1 else ind1 in
               let ind' = if shuffle then list_shuffle ind2 else ind2 in
               Bench.Test.create ~name:(Printf.sprintf "worst/impl:%s/len:%d/rep:%d/maxes:%d/max:%d" i l repeat (List.length ind) d)
                 (fun () -> ignore (List.iter (fun i -> ignore (I.index skip s i)) ind')))
               (impls_of_strings (fix_list impl)))
             (ints_of_strings (fix_list length))))))

(* GraphViz `dot` *)

let dot_list_command : string * Command.t =
  ("list",
    Command.basic ~summary:"print in GraphViz dot format the result of `make_list_ints`"
      (let%map_open length = flag "length" (required int) ~doc:"{LENGTHS} length"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (required string) ~doc:"{IMPLS} implementation name"
                and value = flag "value" (no_arg) ~doc:"show the value in each node"
                and distance = flag "distance" (no_arg) ~doc:"show the distance from the front in each node" in
       fun () ->
         let (i, m) = impl_of_string impl in
         let module I = (val (m : (module Myers.Implementation))) in
         let module G = Myers.Generic (I) in
         let label = Printf.sprintf "make_list_ints [ impl: %s length: %d ]" i length in
         Printf.printf "%s" (G.to_dot label value distance (I.unwrap (G.make_list_ints skip length)))))

let dot_tree_command : string * Command.t =
  ("tree",
    Command.basic ~summary:"print in GraphViz dot format the result of `make_tree_ints`"
      (let%map_open height = flag "height" (required int) ~doc:"HEIGHT height"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (required string) ~doc:"{IMPLS} implementation name"
                and value = flag "value" (no_arg) ~doc:"show the value in each node"
                and distance = flag "distance" (no_arg) ~doc:"show the distance from the front" in
       fun () ->
         let (i, m) = impl_of_string impl in
         let module I = (val (m : (module Myers.Implementation))) in
         let module G = Myers.Generic (I) in
         let label = Printf.sprintf "make_tree_ints [ impl: %s height: %d ]" i height in
         Printf.printf "%s" (G.to_dot label value distance (I.unwrap (G.make_tree_ints skip height)))))

let dot_1lgn_command : string * Command.t =
  ("1lgn",
    Command.basic ~summary:"print in GraphViz dot format the result of `Myers_1lgn.build_tree_0`"
      (let%map_open height = flag "height" (required int) ~doc:"HEIGHT height"
                and skip = flag "skip" (required int) ~doc:"SKIP skip size"
                and path = flag "path" (optional string) ~doc:"SRC,DST highlight the path from SRC to DST"
                and opt_path = flag "opt-path" (optional string) ~doc:"SRC,DST highlight the optimal path from SRC to DST"
                and distance = flag "distance" (no_arg) ~doc:"show the distance from the front in each node"
                and uncle_skip = flag "uncle-skip" (no_arg) ~doc:"show skip uncle edges"
                and uncle_leaf = flag "uncle-leaf" (no_arg) ~doc:"show leaf uncle edges"
                and uncle = flag "uncle" (no_arg) ~doc:"show skip and leaf uncle edges"
                and fringe_inc = flag "fringe-inc" (no_arg) ~doc:"show fringe inc edges"
                and fringe_greater = flag "fringe-greater" (no_arg) ~doc:"show fringe greater edges"
                and fringe = flag "fringe" (no_arg) ~doc:"show fringe inc and greater edges"
                and all = flag "all" (no_arg) ~doc:"show all edges" in
       fun () ->
         let module M = Myers_1lgn in
         let module I = Myers_1lgn.Implementation in
         let module G = Myers_1lgn.Generic in
         let label = Printf.sprintf "build_tree_0 [ impl: %s height: %d ]" "lg1" height in
         let c = M.make_tree_rev skip height 0 (1 -- I.length_of_height skip height) in
         let highlight = match path with
           | Some s -> let (src, dst) = Util.two_ints_of_string s in
                       I.find_path skip (I.lookup skip c src) dst
           | None ->
              match opt_path with
              | Some s -> let (src, dst) = Util.two_ints_of_string s in
                          G.find_opt_path skip (I.lookup skip c src) dst
              | None -> [] in
         Printf.printf "%s"
           (M.tree_to_dot
              skip label highlight distance
              (all || uncle || uncle_skip) (all || uncle || uncle_leaf)
              (all || fringe || fringe_inc) (all || fringe || fringe_greater)
              c)))

(* Distance *)

let distance_list_command : string * Command.t =
  ("list",
    Command.basic ~summary:"print distance-from-front metrics for the result of `make_list_ints`"
      (let%map_open length = flag "length" (one_or_more string) ~doc:"{LENGTHS} length"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name"
                and opt = flag "opt" (no_arg) ~doc:"use optimal distances"
                and path = flag "path" (no_arg) ~doc:"use distances from `find_path`"
                and dist = flag "dist" (no_arg) ~doc:"also show distribution of distances"
                and array = flag "array" (no_arg) ~doc:"also show actual distances"
                and warn = flag "warn" (no_arg) ~doc:"warn when taking .jump is not optimal" in
       fun () ->
         List.iter (fun (i, m) ->
           List.iter (fun l ->
             let module I = (val (m : (module Myers.Implementation))) in
             let module G = Myers.Generic (I) in
             let c = I.unwrap (G.make_list_ints skip l) in
             let opt_array = if opt then G.distance warn c else Array.make 0 0 in
             let path_array = if path then Array.init (I.length c + 1) (fun i -> List.length (I.find_path skip c i) - 1) else Array.make 0 0 in
             Printf.printf "distance from front [ impl: %s skip: %d length: %d ]:" i skip l;
             if path then Printf.printf " path_max: %d path_avg: %.2f" (array_max path_array 0) (array_avg path_array);
             if opt then Printf.printf " opt_max: %d opt_avg: %.2f" (array_max opt_array 0) (array_avg opt_array);
             if dist then (
               if path then Printf.printf " path_dist: %s" (Util.format_array Util.format_int (array_distribution path_array));
               if opt then Printf.printf " opt_dist: %s" (Util.format_array Util.format_int (array_distribution opt_array)));
             if array then (
               if path then Printf.printf " path_array: %s" (Util.format_array Util.format_int path_array);
               if opt then Printf.printf " opt_array: %s" (Util.format_array Util.format_int opt_array));
             Printf.printf "\n")
             (ints_of_strings (fix_list length)))
           (impls_of_strings (fix_list impl))))

let distance_tree_command : string * Command.t =
  ("tree",
    Command.basic ~summary:"print distance-from-front metrics for the result of `make_tree_ints`"
      (let%map_open height = flag "height" (one_or_more string) ~doc:"{HEIGHTS} height"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name"
                and opt = flag "opt" (no_arg) ~doc:"use optimal distances"
                and path = flag "path" (no_arg) ~doc:"use distances from `find_path`"
                and dist = flag "dist" (no_arg) ~doc:"also show distribution of results"
                and array = flag "array" (no_arg) ~doc:"also show actual results"
                and warn = flag "warn" (no_arg) ~doc:"warn when taking .jump is not optimal" in
       fun () ->
         List.iter (fun (i, m) ->
           List.iter (fun h ->
             let module I = (val (m : (module Myers.Implementation))) in
             let module G = Myers.Generic (I) in
             let c = I.unwrap (G.make_tree_ints skip h) in
             let opt_array = if opt then G.distance warn c else Array.make 0 0 in
             let path_array = if path then Array.init (I.length c + 1) (fun i -> List.length (I.find_path skip c i) - 1) else Array.make 0 0 in
             Printf.printf "distance from front [ impl: %s skip: %d height: %d len: %d ]:" i skip h (I.length c);
             if path then Printf.printf " path_max: %d path_avg: %.2f" (array_max path_array 0) (array_avg path_array);
             if opt then Printf.printf " opt_max: %d opt_avg: %.2f" (array_max opt_array 0) (array_avg opt_array);
             if dist then (
               if path then Printf.printf " path_dist: %s" (Util.format_array Util.format_int (array_distribution path_array));
               if opt then Printf.printf " opt_dist: %s" (Util.format_array Util.format_int (array_distribution opt_array)));
             if array then (
               if path then Printf.printf " path_array: %s" (Util.format_array Util.format_int path_array);
               if opt then Printf.printf " opt_array: %s" (Util.format_array Util.format_int opt_array));
             Printf.printf "\n")
             (ints_of_strings (fix_list height)))
           (impls_of_strings (fix_list impl))))

(* Paths *)

let path_command : string * Command.t =
  ("path",
    Command.basic ~summary:"Compute the path between two cells"
      (let%map_open skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and src = flag "src" (one_or_more string) ~doc:"{SRCS} source position"
                and dst = flag "dst" (one_or_more string) ~doc:"{DSTS} destination position"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name"
                and opt = flag "opt" (no_arg) ~doc:"use optimal distances"
                and path = flag "path" (no_arg) ~doc:"use distances from `find_path`" in
       fun () ->
         List.iter (fun (i, m) ->
           let module I = (val (m : (module Myers.Implementation))) in
           let module G = Myers.Generic (I) in
           List.iter (fun s ->
             List.iter (fun t ->
               if s >= t then
                 let v :: vs = 0 -- s [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
                 let c : int I.t = I.unwrap (I.make_list_rev skip v vs) in
                 if path then (
                   let p : int I.t list = I.find_path skip c t in
                   Printf.printf "impl_path [ impl: %s skip: %d source: %d target: %d path-length: %d ]: " i skip s t (List.length p - 1);
                   List.iter (fun c -> Printf.printf " %d" (I.length c)) p;
                   Printf.printf "\n%!";
                   if not (I.length (List.hd (List.rev p)) == s) then failwith "path last doesn't match source";
                   if not (I.length (List.hd p) == t) then failwith "path head doesn't match target";
                   if not (G.check_path s p) then failwith "path not connected");
                 if opt then (
                   let d : int array = G.distance false c in
                   let p : int I.t list = G.find_opt_path skip c t in
                   Printf.printf "opt_path  [ impl: %s skip: %d source: %d target: %d path-length: %d ]: " i skip s t d.(t);
                   List.iter (fun c -> Printf.printf " %d" (I.length c)) p;
                   Printf.printf "\n%!"))
               (ints_of_strings (fix_list dst)))
             (ints_of_strings (fix_list src)))
           (impls_of_strings (fix_list impl))))

(* Tests *)

let test_tree_list_command : string * Command.t =
  ("make-tree=make-list",
    Command.basic ~summary:"test that `make_tree_rev` and `make_list_rev` match"
      (let%map_open height = flag "height" (one_or_more string) ~doc:"{HEIGHTS} height"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name" in
       fun () ->
         List.iter (fun (i, m) ->
           let module I = (val (m : (module Myers.Implementation))) in
           let module G = Myers.Generic (I) in
           List.iter (fun h ->
             Printf.printf "test.make-tree/make-list [ impl: %s height: %d ]: %B\n%!" i h (G.test_tree_list skip h))
           (ints_of_strings (fix_list height)))
         (impls_of_strings (fix_list impl))))

let test_length_of_height_command : string * Command.t =
  ("length-of-height",
    Command.basic ~summary:"test that `length_of_height` matches `make_tree_rev`"
      (let%map_open height = flag "height" (one_or_more string) ~doc:"{HEIGHTS} height"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name" in
       fun () ->
         List.iter (fun (i, m) ->
           let module I = (val (m : (module Myers.Implementation))) in
           let module G = Myers.Generic (I) in
           List.iter (fun h ->
             Printf.printf "test.length-of-height [ impl: %s skip: %d height: %d ]: %B\n%!" i skip h (G.test_length_of_height skip h))
           (ints_of_strings (fix_list height)))
         (impls_of_strings (fix_list impl))))

let test_find_index_command : string * Command.t =
  ("find=index",
    Command.basic ~summary:"test that `find` matches `index`"
      (let%map_open length = flag "length" (one_or_more string) ~doc:"{LENGTHS} length"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name" in
       fun () ->
         List.iter (fun (i, m) ->
           let module I = (val (m : (module Myers.Implementation))) in
           let module G = Myers.Generic (I) in
           List.iter (fun l ->
             Printf.printf "test.find/index [ impl: %s skip: %d length: %d ]: %B\n%!" i skip l (G.test_find_index skip l))
           (ints_of_strings (fix_list length)))
         (impls_of_strings (fix_list impl))))

let test_find_path_index_path_command : string * Command.t =
  ("find-path=index-path",
    Command.basic ~summary:"test that `find_path` matches `index_path`"
      (let%map_open length = flag "length" (one_or_more string) ~doc:"{LENGTHS} length"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name" in
       fun () ->
         List.iter (fun (i, m) ->
           let module I = (val (m : (module Myers.Implementation))) in
           let module G = Myers.Generic (I) in
           List.iter (fun l ->
             Printf.printf "test.find-path/index-path [ impl: %s skip: %d length: %d ]: %B\n%!" i skip l (G.test_find_path_index_path skip l))
           (ints_of_strings (fix_list length)))
         (impls_of_strings (fix_list impl))))

let test_find_find_path_command : string * Command.t =
  ("find=find-path",
    Command.basic ~summary:"test that `find` matches `find_path`"
      (let%map_open length = flag "length" (one_or_more string) ~doc:"{LENGTHS} length"
                and skip = flag "skip" (optional_with_default 2 int) ~doc:"SKIP skip size"
                and impl = flag "impl" (one_or_more string) ~doc:"{IMPLS} implementation name" in
       fun () ->
         List.iter (fun (i, m) ->
           let module I = (val (m : (module Myers.Implementation))) in
           let module G = Myers.Generic (I) in
           List.iter (fun l ->
             Printf.printf "test.find/find-path [ impl: %s skip: %d length: %d ]: %B\n%!" i skip l (G.test_find_find_path skip l))
           (ints_of_strings (fix_list length)))
         (impls_of_strings (fix_list impl))))

let () = Command.run (Command.group ~summary:"Implementation of improved and advanced Myers lists" [
  ("bench", Command.group ~summary:"Run benchmarks" [
    bench_make_list_command;
    bench_make_tree_command;
    bench_index_command;
    bench_worst_command]);

  ("dot", Command.group ~summary:"Generate GraphViz graphs of the list structure" [
    dot_list_command;
    dot_tree_command;
    dot_1lgn_command]);

  ("distance", Command.group ~summary:"Calculate distances between cells" [
    distance_list_command;
    distance_tree_command]);

  path_command;

  ("test", Command.group ~summary:"Run sanity tests" [
    test_tree_list_command;
    test_length_of_height_command;
    test_find_index_command;
    test_find_path_index_path_command;
    test_find_find_path_command]);
  ])
