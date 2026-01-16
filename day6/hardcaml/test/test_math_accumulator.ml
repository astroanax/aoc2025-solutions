(* test for trash compactor - testbench streams raw grid bytes to hardware *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
module Math_accumulator = Hardcaml_day6.Math_accumulator
module I = Math_accumulator.I
module O = Math_accumulator.O
module Sim = Cyclesim.With_interface (I) (O)

let run_testbench ~is_part2 input =
  let sim = Sim.create (fun i -> Math_accumulator.create (Scope.create ()) i) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  
  let lines = String.split_lines input in
  let lines = List.filter lines ~f:(fun l -> not (String.is_empty (String.strip l))) in
  let width = List.fold lines ~init:0 ~f:(fun acc l -> Int.max acc (String.length l)) in
  let height = List.length lines - 1 in  (* exclude operator row *)
  
  inputs.I.clear := Bits.vdd; cycle (); inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd; cycle (); inputs.start := Bits.gnd;
  
  (* stream grid bytes row by row, padding to width *)
  List.iter lines ~f:(fun line ->
    for x = 0 to width - 1 do
      let byte = if x < String.length line then Char.to_int (String.get line x) else 0x20 in
      inputs.grid_byte := Bits.of_int_trunc ~width:8 byte;
      inputs.grid_valid := Bits.vdd;
      cycle ()
    done);
  inputs.grid_valid := Bits.gnd;
  
  (* signal done with grid dimensions *)
  inputs.width := Bits.of_int_trunc ~width:12 width;
  inputs.height := Bits.of_int_trunc ~width:12 height;
  inputs.is_part2 := if is_part2 then Bits.vdd else Bits.gnd;
  inputs.grid_done := Bits.vdd;
  cycle ();
  inputs.grid_done := Bits.gnd;
  
  (* wait for hardware to complete *)
  let max_cycles = 50000 in
  let cycles = ref 0 in
  while not (Bits.to_bool !(outputs.O.done_)) && !cycles < max_cycles do
    cycle ();
    incr cycles
  done;
  
  Bits.to_int64_trunc !(outputs.result)

let%expect_test "example" =
  let input = {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  |} in
  let part1 = run_testbench ~is_part2:false input in
  let part2 = run_testbench ~is_part2:true input in
  Printf.printf "part1: %Ld (expected 4277556)\n" part1;
  Printf.printf "part2: %Ld (expected 3263827)\n" part2;
  [%expect {|
    part1: 4277556 (expected 4277556)
    part2: 3263827 (expected 3263827)
  |}]

let puzzle_input = 
  In_channel.read_all "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day6/input1.txt"

let%expect_test "puzzle input" =
  let part1 = run_testbench ~is_part2:false puzzle_input in
  let part2 = run_testbench ~is_part2:true puzzle_input in
  Printf.printf "part1: %Ld\n" part1;
  Printf.printf "part2: %Ld\n" part2;
  [%expect {|
    part1: 4583860641327
    part2: 11602774058280
  |}]
