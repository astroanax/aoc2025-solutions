(* interface for day 6 math accumulator *)

open! Hardcaml

val result_bits : int

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; number : 'a
    ; number_valid : 'a
    ; is_multiply : 'a
    ; problem_done : 'a
    ; all_done : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { total : 'a
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
