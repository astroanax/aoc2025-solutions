(* Hardware arithmetic sequence summer. Computes sum = lower*(n+1) + step*n*(n+1)/2 *)

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
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

let arithmetic_sum ~lower ~upper ~step ~n =
  let n_plus_1 = n +:. 1 in
  let term1 = sel_bottom (lower *: n_plus_1) ~width:data_bits in
  let n_times_np1 = sel_bottom (n *: n_plus_1) ~width:data_bits in
  let triangular = srl n_times_np1 ~by:1 in
  let term2 = sel_bottom (step *: triangular) ~width:data_bits in
  let valid = lower <=: upper in
  mux2 valid (term1 +: term2) (zero data_bits)
;;

let create scope ({ clock; clear; start; lower; upper; step; n; 
                     input_valid; finish } : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  
  let%hw_var result = Variable.reg spec ~width:data_bits in
  let%hw_var done_flag = Variable.reg spec ~width:1 in
  let pair_sum = arithmetic_sum ~lower ~upper ~step ~n in
  
  compile [
    when_ start [
      result <--. 0;
      done_flag <--. 0;
    ];
    when_ input_valid [
      result <-- (result.value +: pair_sum);
    ];
    when_ finish [
      done_flag <--. 1;
    ];
  ];
  
  { result = result.value
  ; result_valid = done_flag.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"gift_shop" create
;;
