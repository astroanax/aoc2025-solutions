(* Math accumulator for streaming arithmetic with + and * operators.
   Testbench parses grid and sends pre-computed numbers to hardware. *)

open! Core
open! Hardcaml
open! Signal

let result_bits = 64

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a               
    ; number : 'a [@bits result_bits]
    ; number_valid : 'a        
    ; is_multiply : 'a         
    ; problem_done : 'a        
    ; all_done : 'a            
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { total : 'a [@bits result_bits]
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | Processing | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  
  (* problem_acc accumulates within a problem, grand_total across all problems *)
  let%hw_var problem_acc = Variable.reg spec ~width:result_bits in
  let%hw_var grand_total = Variable.reg spec ~width:result_bits in
  let%hw_var first_number = Variable.reg spec ~width:1 in
  let%hw_var done_flag = Variable.reg spec ~width:1 in
  
  compile [
    sm.switch [
      (Idle, [ when_ i.start [
        grand_total <--. 0; problem_acc <--. 0; first_number <--. 1;
        done_flag <--. 0; sm.set_next Processing; ]; ]);
      
      (Processing, [
        when_ i.number_valid [
          (* first number initializes accumulator, subsequent numbers apply operator *)
          if_ first_number.value [
            problem_acc <-- i.number; first_number <--. 0;
          ] [
            if_ i.is_multiply [
              problem_acc <-- sel_bottom (problem_acc.value *: i.number) ~width:result_bits;
            ] [
              problem_acc <-- (problem_acc.value +: i.number);
            ]; ]; ];
        when_ i.problem_done [
          grand_total <-- grand_total.value +: problem_acc.value;
          problem_acc <--. 0; first_number <--. 1; ];
        when_ i.all_done [ done_flag <--. 1; sm.set_next Done; ]; ]);
      
      (Done, [ when_ i.start [ sm.set_next Idle ]; ]); ]; ];
  
  { O.total = grand_total.value; result_valid = done_flag.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"math_accumulator" create
