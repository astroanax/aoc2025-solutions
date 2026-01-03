(* Test for battery_selector module *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Battery_selector = Hardcaml_demo_project.Battery_selector
module I = Battery_selector.I
module O = Battery_selector.O
module Harness = Cyclesim_harness.Make (Battery_selector.I) (Battery_selector.O)

let ( <-- ) signal bits = signal := bits

let run_testbench ~n_select banks (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  
  List.iter banks ~f:(fun bank ->
    let len = String.length bank in
    inputs.bank_length <-- Bits.of_int_trunc ~width:8 len;
    inputs.set_length := Bits.vdd;
    cycle ();
    inputs.set_length := Bits.gnd;
    
    String.iter bank ~f:(fun ch ->
      let digit = Char.to_int ch - Char.to_int '0' in
      inputs.digit <-- Bits.of_int_trunc ~width:4 digit;
      inputs.digit_valid := Bits.vdd;
      cycle ();
      inputs.digit_valid := Bits.gnd;
    );
    
    inputs.finish_bank := Bits.vdd;
    cycle ();
    inputs.finish_bank := Bits.gnd;
    for _ = 1 to n_select + 2 do cycle () done;
  );
  
  inputs.finish_all := Bits.vdd;
  cycle ();
  inputs.finish_all := Bits.gnd;
  cycle ();
  
  Bits.to_int64_trunc !(outputs.result)
;;

let waves_config = Waves_config.no_waves

let%expect_test "example" =
  let banks = ["987654321111111"; "811111111111119"; "234234234234278"; "818181911112111"] in
  let result =
    Harness.run_advanced ~waves_config ~create:Battery_selector.hierarchical
      (run_testbench ~n_select:2 banks)
  in
  Printf.printf "total: %Ld (expected 357)\n" result;
  [%expect {| total: 357 (expected 357) |}]
;;

let puzzle_input = 
  In_channel.read_lines "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day3/input1.txt"
  |> List.filter ~f:(fun s -> not (String.is_empty s))
;;

let%expect_test "part 1" =
  let result =
    Harness.run_advanced ~waves_config ~create:Battery_selector.hierarchical
      (run_testbench ~n_select:2 puzzle_input)
  in
  Printf.printf "part1: %Ld\n" result;
  [%expect {| part1: 17316 |}]
;;

let%expect_test "part 2" =
  let result =
    Harness.run_advanced ~waves_config ~create:Battery_selector.hierarchical_part2
      (run_testbench ~n_select:12 puzzle_input)
  in
  Printf.printf "part2: %Ld\n" result;
  [%expect {| part2: 171741365473332 |}]
;;
