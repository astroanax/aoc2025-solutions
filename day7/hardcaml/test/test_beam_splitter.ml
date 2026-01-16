(* test for beam_splitter module - computes reachable count in testbench,
   hardware does the timeline dp *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Beam_splitter = Hardcaml_day7.Beam_splitter
module I = Beam_splitter.I
module O = Beam_splitter.O
module Harness = Cyclesim_harness.Make (Beam_splitter.I) (Beam_splitter.O)

(* parse grid, count reachable splitters using dfs *)
let parse_and_count input =
  let lines = String.split_lines input |> List.filter ~f:(fun l -> not (String.is_empty l)) in
  let rows = List.length lines in
  let cols = if rows > 0 then String.length (List.hd_exn lines) else 0 in
  let grid = Array.of_list lines in
  
  let start_col = ref 0 in
  let all_splitters = ref [] in
  
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let ch = String.get grid.(r) c in
      if Char.equal ch 'S' then start_col := c;
      if Char.equal ch '^' then all_splitters := (r, c) :: !all_splitters
    done
  done;
  
  (* count reachable splitters using dfs *)
  let visited = Hashtbl.create (module struct
    type t = int * int [@@deriving sexp, hash, compare]
  end) in
  
  let rec count_from start_row col =
    let rec find_next r =
      if r >= rows then None
      else if Char.equal (String.get grid.(r) col) '^' then Some (r, col)
      else find_next (r + 1)
    in
    match find_next (start_row + 1) with
    | None -> ()
    | Some (r, c) ->
      if not (Hashtbl.mem visited (r, c)) then begin
        Hashtbl.set visited ~key:(r, c) ~data:();
        if c > 0 then count_from r (c - 1);
        if c + 1 < cols then count_from r (c + 1)
      end
  in
  count_from 0 !start_col;
  
  let reachable = Hashtbl.length visited in
  (cols, !start_col, List.rev !all_splitters, reachable)
;;

let run_testbench input (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  
  let (cols, start_col, splitters, reachable) = parse_and_count input in
  
  inputs.clear := Bits.vdd; cycle (); inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd; cycle (); inputs.start := Bits.gnd;
  
  inputs.num_cols := Bits.of_int_trunc ~width:8 cols;
  inputs.start_col := Bits.of_int_trunc ~width:8 start_col;
  inputs.reachable_count := Bits.of_int_trunc ~width:32 reachable;
  inputs.set_dims := Bits.vdd;
  cycle ();
  inputs.set_dims := Bits.gnd;
  
  List.iter splitters ~f:(fun (r, c) ->
    inputs.splitter_row := Bits.of_int_trunc ~width:8 r;
    inputs.splitter_col := Bits.of_int_trunc ~width:8 c;
    inputs.splitter_valid := Bits.vdd;
    cycle ();
    inputs.splitter_valid := Bits.gnd;
  );
  
  inputs.finish_load := Bits.vdd;
  cycle ();
  inputs.finish_load := Bits.gnd;
  
  let max_cycles = 10000000 in
  let cycles = ref 0 in
  while not (Bits.to_bool !(outputs.result_valid)) && !cycles < max_cycles do
    cycle ();
    incr cycles
  done;
  
  if !cycles >= max_cycles then
    Printf.printf "WARNING: timeout after %d cycles\n" max_cycles;
  
  (Bits.to_int_trunc !(outputs.part1), Bits.to_int64_trunc !(outputs.part2))
;;

let waves_config = Waves_config.no_waves

let%expect_test "simple test" =
  let input = {|...S...
.......
...^...
.......
..^.^..
.......|} in
  let (part1, part2) =
    Harness.run_advanced ~waves_config ~create:Beam_splitter.hierarchical (run_testbench input)
  in
  Printf.printf "part1: %d (expected 3)\npart2: %Ld (expected 4)\n" part1 part2;
  [%expect {|
    part1: 3 (expected 3)
    part2: 4 (expected 4)
  |}]
;;

let%expect_test "example" =
  let input = {|.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............|} in
  let (part1, part2) =
    Harness.run_advanced ~waves_config ~create:Beam_splitter.hierarchical (run_testbench input)
  in
  Printf.printf "part1: %d (expected 21)\npart2: %Ld (expected 40)\n" part1 part2;
  [%expect {|
    part1: 21 (expected 21)
    part2: 40 (expected 40)
  |}]
;;

let puzzle_input = 
  In_channel.read_all "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day7/input1.txt"
;;

let%expect_test "puzzle input" =
  let (part1, part2) =
    Harness.run_advanced ~waves_config ~create:Beam_splitter.hierarchical (run_testbench puzzle_input)
  in
  Printf.printf "part1: %d\npart2: %Ld\n" part1 part2;
  [%expect {|
    part1: 1560
    part2: 25592971184998
  |}]
;;
