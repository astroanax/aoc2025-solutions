(* Test for safe_dial module *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Safe_dial = Hardcaml_demo_project.Safe_dial
module I = Safe_dial.I
module O = Safe_dial.O
module Harness = Cyclesim_harness.Make (Safe_dial.I) (Safe_dial.O)

let ( <--. ) = Bits.( <--. )

let parse_instruction str =
  let direction = String.get str 0 in
  let distance = String.sub str ~pos:1 ~len:(String.length str - 1) |> Int.of_string in
  let dir_bit = match direction with
    | 'L' -> 0
    | 'R' -> 1
    | _ -> failwith "Invalid direction"
  in
  (dir_bit, distance)
;;

let run_testbench instructions (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  cycle ();
  
  List.iter instructions ~f:(fun instr ->
    let (dir, dist) = parse_instruction instr in
    inputs.direction <--. dir;
    inputs.distance <--. dist;
    inputs.instruction_valid := Bits.vdd;
    cycle ();
    inputs.instruction_valid := Bits.gnd;
    cycle ();
  );
  
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  
  while not (Bits.to_bool !(outputs.result_valid)) do
    cycle ()
  done;
  
  let zero_count = Bits.to_unsigned_int !(outputs.zero_count) in
  let wrap_count = Bits.to_unsigned_int !(outputs.wrap_count) in
  (zero_count, wrap_count)
;;

let waves_config = Waves_config.no_waves

let example_instructions = 
  [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" ]

let%expect_test "example" =
  let (zero_count, wrap_count) =
    Harness.run_advanced 
      ~waves_config 
      ~create:Safe_dial.hierarchical 
      (run_testbench example_instructions)
  in
  Printf.printf "part1: %d (expected 3)\n" zero_count;
  Printf.printf "part2: %d (expected 6)\n" wrap_count;
  [%expect {|
    part1: 3 (expected 3)
    part2: 6 (expected 6)
  |}]
;;

let puzzle_instructions = 
  In_channel.read_lines "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day1/input1.txt"
  |> List.filter ~f:(fun line -> not (String.is_empty (String.strip line)))
;;

let%expect_test "puzzle input" =
  let (zero_count, wrap_count) =
    Harness.run_advanced 
      ~waves_config 
      ~create:Safe_dial.hierarchical 
      (run_testbench puzzle_instructions)
  in
  Printf.printf "part1: %d\n" zero_count;
  Printf.printf "part2: %d\n" wrap_count;
  [%expect {|
    part1: 1158
    part2: 6860
  |}]
;;
