(* gift shop - hardware accumulates arithmetic sequence sums.

   the problem finds digit ids with repeating patterns. for each pattern type,
   valid patterns form an arithmetic sequence. testbench computes intersection
   parameters (lower, upper, step, n) for each pattern/range pair, which requires
   division/modulo that is expensive in hardware.
   
   hardware receives these parameters and computes the triangular sum formula:
   sum = lower*(n+1) + step*n*(n+1)/2
   this involves multiple 64-bit multiplications which benefit from hardware. *)

open! Core
open! Hardcaml
open! Signal

let data_bits = 64

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; lower : 'a [@bits data_bits]
    ; upper : 'a [@bits data_bits]  
    ; step : 'a [@bits data_bits]
    ; n : 'a [@bits data_bits]
    ; input_valid : 'a
    ; finish : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { result : 'a [@bits data_bits]
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

(* compute sum of arithmetic sequence: lower + (lower+step) + ... + upper
   = lower*(n+1) + step*(0+1+2+...+n) = lower*(n+1) + step*n*(n+1)/2 *)
let arithmetic_sum ~lower ~upper ~step ~n =
  let n_plus_1 = n +:. 1 in
  let term1 = sel_bottom (lower *: n_plus_1) ~width:data_bits in
  let n_times_np1 = sel_bottom (n *: n_plus_1) ~width:data_bits in
  let triangular = srl n_times_np1 ~by:1 in
  let term2 = sel_bottom (step *: triangular) ~width:data_bits in
  let valid = lower <=: upper in
  mux2 valid (term1 +: term2) (zero data_bits)

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  
  let%hw_var result = Variable.reg spec ~width:data_bits in
  let%hw_var done_flag = Variable.reg spec ~width:1 in
  let pair_sum = arithmetic_sum ~lower:i.lower ~upper:i.upper ~step:i.step ~n:i.n in
  
  compile [
    when_ i.start [
      result <--. 0;
      done_flag <--. 0;
    ];
    when_ i.input_valid [
      result <-- (result.value +: pair_sum);
    ];
    when_ i.finish [
      done_flag <--. 1;
    ];
  ];
  
  { O.result = result.value; done_ = done_flag.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"gift_shop" create
