(* test for circuit_solver module - streams sorted edges to hardware union-find *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Circuit_solver = Hardcaml_day8.Circuit_solver
module I = Circuit_solver.I
module O = Circuit_solver.O
module Harness = Cyclesim_harness.Make (Circuit_solver.I) (Circuit_solver.O)

(* parse points and compute sorted edges *)
let parse_and_sort input =
  let points = String.split_lines input
    |> List.filter ~f:(fun l -> not (String.is_empty l))
    |> List.map ~f:(fun l ->
      match String.split l ~on:',' |> List.map ~f:(fun s -> Int.of_string (String.strip s)) with
      | [x; y; z] -> (x, y, z)
      | _ -> failwith "bad input")
    |> Array.of_list
  in
  let n = Array.length points in
  
  (* compute all pairwise squared distances *)
  let edges = ref [] in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let (x1, y1, z1) = points.(i) in
      let (x2, y2, z2) = points.(j) in
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      let dz = z1 - z2 in
      let dist_sq = dx*dx + dy*dy + dz*dz in
      edges := (dist_sq, i, j, x1, x2) :: !edges
    done
  done;
  
  let sorted_edges = List.sort !edges ~compare:(fun (d1, _, _, _, _) (d2, _, _, _, _) -> 
    Int.compare d1 d2) in
  (n, sorted_edges)
;;

let run_testbench input (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  
  let (n, sorted_edges) = parse_and_sort input in
  
  (* reset and set number of nodes *)
  inputs.clear := Bits.vdd; cycle (); inputs.clear := Bits.gnd;
  inputs.num_nodes := Bits.of_int_trunc ~width:11 n;
  inputs.set_num_nodes := Bits.vdd;
  cycle ();
  inputs.set_num_nodes := Bits.gnd;
  
  (* start *)
  inputs.start := Bits.vdd; cycle (); inputs.start := Bits.gnd;
  
  (* wait for init *)
  for _ = 1 to n + 5 do cycle () done;
  
  (* stream edges *)
  List.iter sorted_edges ~f:(fun (_, a, b, x1, x2) ->
    inputs.edge_a := Bits.of_int_trunc ~width:11 a;
    inputs.edge_b := Bits.of_int_trunc ~width:11 b;
    inputs.edge_x1 := Bits.of_int_trunc ~width:32 x1;
    inputs.edge_x2 := Bits.of_int_trunc ~width:32 x2;
    inputs.edge_valid := Bits.vdd;
    cycle ();
    inputs.edge_valid := Bits.gnd;
    (* wait for union operation to complete - up to ~log(n) cycles for find *)
    for _ = 1 to 30 do cycle () done;
  );
  
  (* finish *)
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  
  (Bits.to_int64_trunc !(outputs.part1), Bits.to_int64_trunc !(outputs.part2))
;;

let waves_config = Waves_config.no_waves

let%expect_test "example" =
  let input = {|162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689|} in
  let (part1, part2) =
    Harness.run_advanced ~waves_config ~create:Circuit_solver.hierarchical 
      (run_testbench input)
  in
  Printf.printf "part1: %Ld\npart2: %Ld\n" part1 part2;
  [%expect {|
    part1: 0
    part2: 25272
  |}]
;;

let puzzle_input = 
  In_channel.read_all "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day8/input1.txt"
;;

let%expect_test "puzzle input" =
  let (part1, part2) =
    Harness.run_advanced ~waves_config ~create:Circuit_solver.hierarchical 
      (run_testbench puzzle_input)
  in
  Printf.printf "part1: %Ld\npart2: %Ld\n" part1 part2;
  [%expect {|
    part1: 50568
    part2: 36045012
  |}]
;;
