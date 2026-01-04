(* Test for roll_counter module *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Roll_counter = Hardcaml_day4.Roll_counter
module I = Roll_counter.I
module O = Roll_counter.O
module Harness = Cyclesim_harness.Make (Roll_counter.I) (Roll_counter.O)

let dirs = [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)]

let parse_grid input =
  let lines = String.split_lines input |> List.filter ~f:(fun s -> not (String.is_empty s)) in
  let rows = List.length lines in
  let cols = if rows > 0 then String.length (List.hd_exn lines) else 0 in
  let grid = Array.of_list lines in
  let is_roll r c =
    if r >= 0 && r < rows && c >= 0 && c < cols then
      Char.equal (String.get grid.(r) c) '@'
    else false
  in
  let neighbor_count r c = List.count dirs ~f:(fun (dr, dc) -> is_roll (r + dr) (c + dc)) in
  (rows, cols, is_roll, neighbor_count)

let run_testbench input (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  let (rows, cols, is_roll, neighbor_count) = parse_grid input in
  
  inputs.clear := Bits.vdd; cycle (); inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd; cycle (); inputs.start := Bits.gnd;
  inputs.rows := Bits.of_int_trunc ~width:8 rows;
  inputs.cols := Bits.of_int_trunc ~width:8 cols;
  inputs.set_dims := Bits.vdd; cycle (); inputs.set_dims := Bits.gnd;
  
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let roll = is_roll r c in
      let count = if roll then neighbor_count r c else 0 in
      inputs.cell_row := Bits.of_int_trunc ~width:8 r;
      inputs.cell_col := Bits.of_int_trunc ~width:8 c;
      inputs.cell_is_roll := Bits.of_int_trunc ~width:1 (if roll then 1 else 0);
      inputs.cell_neighbor_count := Bits.of_int_trunc ~width:4 count;
      inputs.cell_valid := Bits.vdd; cycle (); inputs.cell_valid := Bits.gnd;
    done
  done;
  
  inputs.finish_load := Bits.vdd; cycle (); inputs.finish_load := Bits.gnd;
  for _ = 1 to 500000 do
    if not (Bits.to_bool !(outputs.result_valid)) then cycle ()
  done;
  
  (Bits.to_int_trunc !(outputs.part1), Bits.to_int_trunc !(outputs.part2))
;;

let waves_config = Waves_config.no_waves

let%expect_test "example" =
  let input = {|
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
|} in
  let (part1, part2) =
    Harness.run_advanced ~waves_config ~create:Roll_counter.hierarchical (run_testbench input)
  in
  Printf.printf "part1: %d (expected 13)\npart2: %d (expected 43)\n" part1 part2;
  [%expect {|
    part1: 13 (expected 13)
    part2: 43 (expected 43)
  |}]
;;

let puzzle_input = 
  In_channel.read_all "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day4/input1.txt"
;;

let%expect_test "puzzle input" =
  let (part1, part2) =
    Harness.run_advanced ~waves_config ~create:Roll_counter.hierarchical (run_testbench puzzle_input)
  in
  Printf.printf "part1: %d\npart2: %d\n" part1 part2;
  [%expect {|
    part1: 1416
    part2: 9086
  |}]
;;
