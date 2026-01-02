(* Hardware implementation of the safe dial puzzle. Uses combinational divider
   for single-cycle per instruction processing. *)

open! Core
open! Hardcaml
open! Signal

let dial_bits = 7
let distance_bits = 10

(* Combinational divider: compute quotient and remainder of x / 100 using comparison tree. *)
let div_mod_100 x =
  let open Signal in
  let width_x = width x in
  
  let q0 = x <:. 100 in
  let q1 = x <:. 200 in
  let q2 = x <:. 300 in
  let q3 = x <:. 400 in
  let q4 = x <:. 500 in
  let q5 = x <:. 600 in
  let q6 = x <:. 700 in
  let q7 = x <:. 800 in
  let q8 = x <:. 900 in
  let q9 = x <:. 1000 in
  let q10 = x <:. 1100 in
  let q11 = x <:. 1200 in
  
  let quotient =
    mux2 q0 (of_int_trunc ~width:16 0)
      (mux2 q1 (of_int_trunc ~width:16 1)
        (mux2 q2 (of_int_trunc ~width:16 2)
          (mux2 q3 (of_int_trunc ~width:16 3)
            (mux2 q4 (of_int_trunc ~width:16 4)
              (mux2 q5 (of_int_trunc ~width:16 5)
                (mux2 q6 (of_int_trunc ~width:16 6)
                  (mux2 q7 (of_int_trunc ~width:16 7)
                    (mux2 q8 (of_int_trunc ~width:16 8)
                      (mux2 q9 (of_int_trunc ~width:16 9)
                        (mux2 q10 (of_int_trunc ~width:16 10)
                          (mux2 q11 (of_int_trunc ~width:16 11)
                            (of_int_trunc ~width:16 12))))))))))))
  in
  
  let q_times_100 =
    mux2 q0 (of_int_trunc ~width:width_x 0)
      (mux2 q1 (of_int_trunc ~width:width_x 100)
        (mux2 q2 (of_int_trunc ~width:width_x 200)
          (mux2 q3 (of_int_trunc ~width:width_x 300)
            (mux2 q4 (of_int_trunc ~width:width_x 400)
              (mux2 q5 (of_int_trunc ~width:width_x 500)
                (mux2 q6 (of_int_trunc ~width:width_x 600)
                  (mux2 q7 (of_int_trunc ~width:width_x 700)
                    (mux2 q8 (of_int_trunc ~width:width_x 800)
                      (mux2 q9 (of_int_trunc ~width:width_x 900)
                        (mux2 q10 (of_int_trunc ~width:width_x 1000)
                          (mux2 q11 (of_int_trunc ~width:width_x 1100)
                            (of_int_trunc ~width:width_x 1200))))))))))))
  in
  
  let remainder = x -: q_times_100 in
  (quotient, remainder)
;;

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; instruction_valid : 'a
    ; direction : 'a
    ; distance : 'a [@bits distance_bits]
    ; finish : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { dial_position : 'a [@bits dial_bits]
    ; zero_count : 'a [@bits 16]
    ; wrap_count : 'a [@bits 16]
    ; ready : 'a
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; instruction_valid; direction; distance; finish } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  
  let%hw_var dial_position = Variable.reg spec ~width:dial_bits in
  let%hw_var zero_count = Variable.reg spec ~width:16 in
  let%hw_var wrap_count = Variable.reg spec ~width:16 in
  let ready = Variable.wire ~default:gnd () in
  let result_valid = Variable.wire ~default:gnd () in
  
  compile
    [ sm.switch
        [ ( Idle
          , [ ready <-- vdd
            ; when_ start
                [ dial_position <--. 50
                ; zero_count <--. 0
                ; wrap_count <--. 0
                ; sm.set_next Idle
                ]
            ; when_ instruction_valid
                [ let pos = Signal.uresize dial_position.value ~width:16 in
                  let dist = Signal.uresize distance ~width:16 in
                  
                  let pos_plus_dist = Signal.(pos +: dist) in
                  let (wraps_right, next_right) = div_mod_100 pos_plus_dist in
                  
                  let pos_is_zero = Signal.(pos ==:. 0) in
                  let (wraps_left_case1, remainder_case1) = div_mod_100 dist in
                  let temp = Signal.(of_int_trunc ~width:16 100 -: remainder_case1) in
                  let (_, next_left_case1) = div_mod_100 temp in
                  
                  let dist_ge_pos = Signal.(dist >=: pos) in
                  let after_zero = Signal.(dist -: pos) in
                  let (wraps_after, remainder_after) = div_mod_100 after_zero in
                  let wraps_left_case2 = Signal.(wraps_after +: of_int_trunc ~width:16 1) in
                  let next_left_case2 = 
                    Signal.mux2 Signal.(remainder_after ==:. 0)
                      (Signal.of_int_trunc ~width:16 0)
                      Signal.(of_int_trunc ~width:16 100 -: remainder_after)
                  in
                  
                  let next_left_case3 = Signal.(pos -: dist) in
                  let wraps_left_case3 = Signal.of_int_trunc ~width:16 0 in
                  
                  let wraps_left =
                    Signal.mux2 pos_is_zero wraps_left_case1
                      (Signal.mux2 dist_ge_pos wraps_left_case2 wraps_left_case3)
                  in
                  let next_left =
                    Signal.mux2 pos_is_zero next_left_case1
                      (Signal.mux2 dist_ge_pos next_left_case2 next_left_case3)
                  in
                  
                  let next_pos = Signal.mux2 direction next_right next_left in
                  let num_wraps = Signal.mux2 direction wraps_right wraps_left in
                  let next_pos_trunc = Signal.sel_bottom next_pos ~width:dial_bits in
                  let lands_on_zero = Signal.(next_pos ==:. 0) in
                  
                  proc
                    [ dial_position <-- next_pos_trunc
                    ; when_ lands_on_zero [ zero_count <-- zero_count.value +:. 1 ]
                    ; wrap_count <-- wrap_count.value +: num_wraps
                    ; sm.set_next Idle
                    ]
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; ( Done
          , [ result_valid <-- vdd
            ; when_ start [ sm.set_next Idle ]
            ] )
        ]
    ];
  { dial_position = dial_position.value
  ; zero_count = zero_count.value
  ; wrap_count = wrap_count.value
  ; ready = ready.value
  ; result_valid = result_valid.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"safe_dial" create
;;
