(* Test for freshness_checker module *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Freshness_checker = Hardcaml_day5.Freshness_checker
module I = Freshness_checker.I
module O = Freshness_checker.O
module Harness = Cyclesim_harness.Make (Freshness_checker.I) (Freshness_checker.O)

let parse_input input =
  let (ranges_section, ids_section) = 
    match String.substr_index input ~pattern:"\n\n" with
    | Some idx -> (String.prefix input idx, String.drop_prefix input (idx + 2))
    | None -> failwith "no blank line separator found"
  in
  let ranges = 
    String.split_lines ranges_section
    |> List.filter ~f:(fun l -> not (String.is_empty l))
    |> List.map ~f:(fun line ->
      match String.lsplit2 line ~on:'-' with
      | Some (a, b) -> (Int64.of_string a, Int64.(of_string b + 1L))
      | None -> failwith ("invalid range: " ^ line))
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int64.compare a b)
  in
  let merged = 
    List.fold ranges ~init:[] ~f:(fun acc (start, end_) ->
      match acc with
      | [] -> [(start, end_)]
      | (prev_start, prev_end) :: rest ->
        if Int64.(start <= prev_end) then (prev_start, Int64.max prev_end end_) :: rest
        else (start, end_) :: acc)
    |> List.rev
  in
  let ids = 
    String.split_lines ids_section
    |> List.filter ~f:(fun l -> not (String.is_empty l))
    |> List.map ~f:Int64.of_string
    |> List.sort ~compare:Int64.compare
  in
  (merged, ids)
;;

let run_testbench input (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  let (merged_ranges, ids) = parse_input input in
  let part2 = List.fold merged_ranges ~init:0L ~f:(fun acc (start, end_) -> Int64.(acc + end_ - start)) in
  
  inputs.clear := Bits.vdd; cycle (); inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd; cycle (); inputs.start := Bits.gnd;
  
  List.iter merged_ranges ~f:(fun (start, end_) ->
    inputs.range_start := Bits.of_int64_trunc ~width:Freshness_checker.id_bits start;
    inputs.range_end := Bits.of_int64_trunc ~width:Freshness_checker.id_bits end_;
    inputs.range_valid := Bits.vdd; cycle (); inputs.range_valid := Bits.gnd);
  
  inputs.ranges_done := Bits.vdd; cycle (); inputs.ranges_done := Bits.gnd;
  
  List.iter ids ~f:(fun id ->
    inputs.check_id := Bits.of_int64_trunc ~width:Freshness_checker.id_bits id;
    inputs.check_valid := Bits.vdd; cycle (); inputs.check_valid := Bits.gnd);
  
  inputs.checking_done := Bits.vdd; cycle (); inputs.checking_done := Bits.gnd; cycle ();
  (Bits.to_int_trunc !(outputs.fresh_count), part2)
;;

let waves_config = Waves_config.no_waves

let puzzle_input = In_channel.read_all "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day5/input1.txt"

let%expect_test "puzzle input" =
  let (part1, part2) =
    Harness.run_advanced ~waves_config ~create:Freshness_checker.hierarchical (run_testbench puzzle_input)
  in
  Printf.printf "part1: %d\npart2: %Ld\n" part1 part2;
  [%expect {|
    part1: 896
    part2: 346240317247002
  |}]
;;
