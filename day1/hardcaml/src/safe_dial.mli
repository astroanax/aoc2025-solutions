open! Core
open! Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; instruction_valid : 'a
    ; direction : 'a
    ; distance : 'a
    ; finish : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { dial_position : 'a
    ; zero_count : 'a
    ; wrap_count : 'a
    ; ready : 'a
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

val div_mod_100 : Signal.t -> Signal.t * Signal.t
val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
