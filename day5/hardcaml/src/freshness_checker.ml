(* Parallel range membership checker - checks ID against all ranges in one cycle. *)

open! Core
open! Hardcaml
open! Signal

let max_ranges = 256
let id_bits = 50

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; range_start : 'a [@bits id_bits]
    ; range_end : 'a [@bits id_bits]
    ; range_valid : 'a
    ; ranges_done : 'a
    ; check_id : 'a [@bits id_bits]
    ; check_valid : 'a
    ; checking_done : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { fresh_count : 'a [@bits 32]
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t = Idle | Loading_ranges | Checking_ids | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  
  let range_ptr_bits = Int.ceil_log2 (max_ranges + 1) in
  let range_starts = Array.init max_ranges ~f:(fun _ -> Variable.reg spec ~width:id_bits) in
  let range_ends = Array.init max_ranges ~f:(fun _ -> Variable.reg spec ~width:id_bits) in
  
  let%hw_var num_ranges = Variable.reg spec ~width:range_ptr_bits in
  let%hw_var range_write_ptr = Variable.reg spec ~width:range_ptr_bits in
  let%hw_var fresh_count = Variable.reg spec ~width:32 in
  let%hw_var done_flag = Variable.reg spec ~width:1 in
  
  let id_in_any_range =
    let checks = List.init max_ranges ~f:(fun idx ->
      let start = range_starts.(idx).value in
      let end_ = range_ends.(idx).value in
      let idx_valid = of_int_trunc ~width:range_ptr_bits idx <: num_ranges.value in
      let in_this_range = (i.check_id >=: start) &: (i.check_id <: end_) in
      idx_valid &: in_this_range
    ) in
    tree ~arity:2 ~f:(reduce ~f:( |: )) checks
  in
  
  compile [
    sm.switch [
      (Idle, [ when_ i.start [
        fresh_count <--. 0; done_flag <--. 0; num_ranges <--. 0; range_write_ptr <--. 0;
        proc (List.init max_ranges ~f:(fun idx ->
          proc [ range_starts.(idx) <--. 0; range_ends.(idx) <--. 0; ]));
        sm.set_next Loading_ranges; ]; ]);
      
      (Loading_ranges, [
        when_ i.range_valid [
          proc (List.mapi (Array.to_list range_starts) ~f:(fun idx v ->
            when_ (range_write_ptr.value ==:. idx) [ v <-- i.range_start ]));
          proc (List.mapi (Array.to_list range_ends) ~f:(fun idx v ->
            when_ (range_write_ptr.value ==:. idx) [ v <-- i.range_end ]));
          range_write_ptr <-- range_write_ptr.value +:. 1; ];
        when_ i.ranges_done [ num_ranges <-- range_write_ptr.value; sm.set_next Checking_ids; ]; ]);
      
      (Checking_ids, [
        when_ i.check_valid [ when_ id_in_any_range [ fresh_count <-- fresh_count.value +:. 1; ]; ];
        when_ i.checking_done [ done_flag <--. 1; sm.set_next Done; ]; ]);
      
      (Done, [ when_ i.start [ sm.set_next Idle ]; ]); ]; ];
  
  { O.fresh_count = fresh_count.value; result_valid = done_flag.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"freshness_checker" create
