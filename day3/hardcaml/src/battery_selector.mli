(* interface for battery_selector module

   the hardware uses a greedy sliding window algorithm to select the N digits 
   that form the maximum possible N-digit number while maintaining their order.
   for each bank, it outputs the N-digit number formed by the selected 
   batteries and accumulates these across all banks. *)

open! Hardcaml

val digit_bits : int

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; bank_length : 'a
    ; set_length : 'a
    ; digit : 'a
    ; digit_valid : 'a
    ; finish_bank : 'a
    ; finish_all : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { result : 'a
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

val create_with_n : n_batteries:int -> Scope.t -> Signal.t I.t -> Signal.t O.t
val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val create_part2 : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical_part2 : Scope.t -> Signal.t I.t -> Signal.t O.t
