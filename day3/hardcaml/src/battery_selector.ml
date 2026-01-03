(* Parallel tree-based max finder for battery selection. O(log n) comparison depth. *)

open! Core
open! Hardcaml
open! Signal

let digit_bits = 4
let max_bank_len = 128

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; bank_length : 'a [@bits 8]
    ; set_length : 'a
    ; digit : 'a [@bits digit_bits]
    ; digit_valid : 'a
    ; finish_bank : 'a
    ; finish_all : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { result : 'a [@bits 64]
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Receiving
    | Selecting
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let rec tree_max_with_index pairs =
  match pairs with
  | [] -> failwith "tree_max_with_index: empty list"
  | [(value, idx, _valid)] -> (value, idx)
  | _ ->
    let rec pair_up = function
      | [] -> []
      | [x] -> [x]
      | (v1, i1, valid1) :: (v2, i2, valid2) :: rest ->
        let take_first = valid1 &: ((~:valid2) |: (v1 >=: v2)) in
        let new_v = mux2 take_first v1 v2 in
        let new_i = mux2 take_first i1 i2 in
        let new_valid = valid1 |: valid2 in
        (new_v, new_i, new_valid) :: pair_up rest
    in
    tree_max_with_index (pair_up pairs)

let create_with_n ~n_batteries scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  
  let n = n_batteries in
  let idx_bits = Int.ceil_log2 (max_bank_len + 1) in
  let digits_mem = Array.init max_bank_len ~f:(fun _ -> Variable.reg spec ~width:digit_bits) in
  
  let%hw_var write_idx = Variable.reg spec ~width:idx_bits in
  let%hw_var bank_len = Variable.reg spec ~width:8 in
  let%hw_var total = Variable.reg spec ~width:64 in
  let%hw_var done_flag = Variable.reg spec ~width:1 in
  let%hw_var cursor = Variable.reg spec ~width:idx_bits in
  let%hw_var remaining = Variable.reg spec ~width:8 in
  let%hw_var accumulator = Variable.reg spec ~width:64 in
  
  let window_end = uresize bank_len.value ~width:idx_bits -: uresize remaining.value ~width:idx_bits +:. 1 in
  
  let digit_signals = Array.to_list digits_mem |> List.map ~f:(fun v -> v.value) in
  let indexed_digits = List.mapi digit_signals ~f:(fun i d ->
    let idx = of_int_trunc ~width:idx_bits i in
    let in_window = (idx >=: cursor.value) &: (idx <: window_end) in
    (d, idx, in_window)
  ) in
  
  let max_digit, max_idx = tree_max_with_index indexed_digits in
  
  compile [
    sm.switch [
      (Idle, [
        when_ i.start [
          total <--. 0;
          done_flag <--. 0;
          sm.set_next Receiving;
        ];
      ]);
      
      (Receiving, [
        when_ i.set_length [
          bank_len <-- uresize i.bank_length ~width:8;
          write_idx <--. 0;
        ];
        when_ i.digit_valid [
          proc (List.mapi (Array.to_list digits_mem) ~f:(fun idx mem ->
            when_ (write_idx.value ==:. idx) [ mem <-- i.digit ]
          ));
          write_idx <-- write_idx.value +:. 1;
        ];
        when_ i.finish_bank [
          cursor <--. 0;
          remaining <--. n;
          accumulator <--. 0;
          sm.set_next Selecting;
        ];
        when_ i.finish_all [
          done_flag <--. 1;
          sm.set_next Done;
        ];
      ]);
      
      (Selecting, [
        if_ (remaining.value >:. 0) [
          accumulator <-- (sel_bottom (accumulator.value *: of_int_trunc ~width:64 10) ~width:64 
                          +: uresize max_digit ~width:64);
          cursor <-- max_idx +:. 1;
          remaining <-- remaining.value -:. 1;
        ] [
          total <-- total.value +: accumulator.value;
          sm.set_next Receiving;
        ];
      ]);
      
      (Done, [
        when_ i.start [ sm.set_next Idle ];
      ]);
    ];
  ];
  
  { O.result = total.value; result_valid = done_flag.value }

let create = create_with_n ~n_batteries:2
let create_part2 = create_with_n ~n_batteries:12

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"battery_selector" create

let hierarchical_part2 scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"battery_selector_part2" create_part2
