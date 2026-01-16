(* interface for day 6 trash compactor - hardware parses grid and applies operators *)

open! Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; grid_byte : 'a
    ; grid_valid : 'a
    ; grid_done : 'a
    ; width : 'a
    ; height : 'a
    ; is_part2 : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { result : 'a
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
