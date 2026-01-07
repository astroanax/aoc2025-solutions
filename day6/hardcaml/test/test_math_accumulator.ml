(* Test using hardcaml_step_testbench effectful API for cleaner testbench code. *)

open! Core
open! Hardcaml
module Math_accumulator = Hardcaml_day6.Math_accumulator
module I = Math_accumulator.I
module O = Math_accumulator.O

module Tb = Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make (I) (O)

(* Parse input grid and extract problems as (is_multiply, numbers) list. *)
let parse_input ~part input =
  let lines = String.split_lines input in
  let lines = List.filter lines ~f:(fun l -> not (String.is_empty (String.strip l))) in
  if List.length lines < 2 then []
  else
    let op_row_idx = List.length lines - 1 in
    let data_rows = List.take lines op_row_idx in
    let op_row = List.nth_exn lines op_row_idx in
    let width = List.fold lines ~init:0 ~f:(fun acc l -> Int.max acc (String.length l)) in
    let height = List.length data_rows in
    
    let grid = Array.init height ~f:(fun y ->
      let line = List.nth_exn data_rows y in
      Array.init width ~f:(fun x ->
        if x < String.length line then String.get line x else ' ')) in
    
    let op_bytes = Array.init width ~f:(fun x ->
      if x < String.length op_row then String.get op_row x else ' ') in
    
    (* scan right-to-left for operators to find problem boundaries *)
    let problems = ref [] in
    let right = ref width in
    for left = width - 1 downto 0 do
      let op = op_bytes.(left) in
      if Char.(op = '+' || op = '*') then begin
        let is_multiply = Char.(op = '*') in
        let numbers = 
          if part = 1 then
            List.init height ~f:(fun y ->
              let num = ref 0L in
              for x = left to !right - 1 do
                let ch = grid.(y).(x) in
                if Char.is_digit ch then
                  num := Int64.(!num * 10L + of_int (Char.get_digit_exn ch))
              done;
              !num)
          else
            (* part 2: column-wise, columns read right-to-left *)
            List.init (!right - left) ~f:(fun col_offset ->
              let x = !right - 1 - col_offset in
              let num = ref 0L in
              for y = 0 to height - 1 do
                let ch = grid.(y).(x) in
                if Char.is_digit ch then
                  num := Int64.(!num * 10L + of_int (Char.get_digit_exn ch))
              done;
              !num)
        in
        problems := (is_multiply, numbers) :: !problems;
        right := left - 1;
      end
    done;
    List.rev !problems
;;

(* Effectful testbench - regular for loops, no monadic style needed. *)
let run_testbench ~part input h _o =
  let problems = parse_input ~part input in
  
  let make_input ?(start=false) ?(number=0L) ?(number_valid=false) 
                 ?(is_multiply=false) ?(problem_done=false) ?(all_done=false) () =
    { I.clock = Bits.gnd
    ; clear = Bits.gnd
    ; start = if start then Bits.vdd else Bits.gnd
    ; number = Bits.of_int64_trunc ~width:Math_accumulator.result_bits number
    ; number_valid = if number_valid then Bits.vdd else Bits.gnd
    ; is_multiply = if is_multiply then Bits.vdd else Bits.gnd
    ; problem_done = if problem_done then Bits.vdd else Bits.gnd
    ; all_done = if all_done then Bits.vdd else Bits.gnd
    }
  in
  
  let _ = Tb.cycle h (make_input ~start:true ()) in
  let _ = Tb.cycle h (make_input ()) in
  
  List.iter problems ~f:(fun (is_multiply, numbers) ->
    List.iter numbers ~f:(fun num ->
      let _ = Tb.cycle h (make_input ~number:num ~number_valid:true ~is_multiply ()) in ());
    let _ = Tb.cycle h (make_input ~problem_done:true ()) in ());
  
  let _ = Tb.cycle h (make_input ~all_done:true ()) in
  let o = Tb.cycle h (make_input ()) in
  Bits.to_int64_trunc (Tb.O_data.after_edge o).total
;;

module Sim = Cyclesim.With_interface (I) (O)

let create_circuit i = Math_accumulator.create (Scope.create ()) i

let run_test ~part input =
  let sim = Sim.create create_circuit in
  Tb.run_until_finished () ~simulator:sim ~testbench:(run_testbench ~part input)
;;

let%expect_test "example" =
  let input = {|123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  |} in
  let part1 = run_test ~part:1 input in
  let part2 = run_test ~part:2 input in
  Printf.printf "part1: %Ld (expected 4277556)\n" part1;
  Printf.printf "part2: %Ld (expected 3263827)\n" part2;
  [%expect {|
    part1: 4277556 (expected 4277556)
    part2: 3263827 (expected 3263827)
  |}]
;;

let puzzle_input = 
  In_channel.read_all "/home/astroanax/dev/aoc2026-janestreet/aoc2026/day6/input1.txt"
;;

let%expect_test "puzzle input" =
  let part1 = run_test ~part:1 puzzle_input in
  let part2 = run_test ~part:2 puzzle_input in
  Printf.printf "part1: %Ld\n" part1;
  Printf.printf "part2: %Ld\n" part2;
  [%expect {|
    part1: 4583860641327
    part2: 11602774058280
  |}]
;;
