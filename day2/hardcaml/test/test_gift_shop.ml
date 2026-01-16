(* Test for gift_shop module *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Gift_shop = Hardcaml_demo_project.Gift_shop
module I = Gift_shop.I
module O = Gift_shop.O
module Harness = Cyclesim_harness.Make (Gift_shop.I) (Gift_shop.O)

let ( <-- ) signal bits = signal := bits

let pattern_params total_digits pattern_size =
  let digits_power = Int64.(pow (of_int 10) (of_int total_digits)) in
  let size_power = Int64.(pow (of_int 10) (of_int pattern_size)) in
  let step = Int64.((digits_power - one) / (size_power - one)) in
  let start = Int64.(step * (size_power / of_int 10)) in
  let end_ = Int64.(step * (size_power - one)) in
  (step, start, end_)
;;

let intersection ~step ~pstart ~pend ~from_ ~to_ =
  let open Int64 in
  let eff_start = max pstart from_ in
  let eff_end = min pend to_ in
  if eff_start > eff_end then (one, zero, zero)
  else
    let remainder = eff_start % step in
    let lower = if remainder = zero then eff_start else eff_start + (step - remainder) in
    let upper = eff_end - (eff_end % step) in
    let lower = max lower pstart in
    if lower > upper then (one, zero, zero)
    else (lower, upper, (upper - lower) / step)
;;

let part1_patterns = [(2, 1); (4, 2); (6, 3); (8, 4); (10, 5)]
let part2_extra = [(3, 1); (5, 1); (6, 2); (7, 1); (9, 3); (10, 2)]
let part2_overlaps = [(6, 1); (10, 1)]

let parse_ranges input =
  String.strip input
  |> String.split ~on:','
  |> List.map ~f:(fun s ->
    let parts = String.split (String.strip s) ~on:'-' in
    (Int64.of_string (List.nth_exn parts 0), Int64.of_string (List.nth_exn parts 1)))
;;

let run_testbench patterns ranges (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  
  List.iter patterns ~f:(fun (total_digits, pattern_size) ->
    let (step, pstart, pend) = pattern_params total_digits pattern_size in
    List.iter ranges ~f:(fun (from_, to_) ->
      let (lower, upper, n) = intersection ~step ~pstart ~pend ~from_ ~to_ in
      inputs.lower <-- Bits.of_int64_trunc ~width:64 lower;
      inputs.upper <-- Bits.of_int64_trunc ~width:64 upper;
      inputs.step <-- Bits.of_int64_trunc ~width:64 step;
      inputs.n <-- Bits.of_int64_trunc ~width:64 n;
      inputs.input_valid := Bits.vdd;
      cycle ();
      inputs.input_valid := Bits.gnd;
    );
  );
  
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  
  Bits.to_int64_trunc !(outputs.result)
;;

let waves_config = Waves_config.no_waves

let puzzle_input = 
  In_channel.read_all "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day2/input1.txt"
;;

let puzzle_ranges = parse_ranges puzzle_input

let%expect_test "part 1" =
  let result =
    Harness.run_advanced ~waves_config ~create:Gift_shop.hierarchical
      (run_testbench part1_patterns puzzle_ranges)
  in
  Printf.printf "part1: %Ld\n" result;
  [%expect {| part1: 35367539282 |}]
;;

let%expect_test "part 2" =
  let part1_sum =
    Harness.run_advanced ~waves_config ~create:Gift_shop.hierarchical
      (run_testbench part1_patterns puzzle_ranges)
  in
  let extra_sum =
    Harness.run_advanced ~waves_config ~create:Gift_shop.hierarchical
      (run_testbench part2_extra puzzle_ranges)
  in
  let overlap_sum =
    Harness.run_advanced ~waves_config ~create:Gift_shop.hierarchical
      (run_testbench part2_overlaps puzzle_ranges)
  in
  let result = Int64.(part1_sum + extra_sum - overlap_sum) in
  Printf.printf "part2: %Ld\n" result;
  [%expect {| part2: 45814076230 |}]
;;
