(* interface for freshness_checker module *)

open! Hardcaml

val max_ranges : int
val id_bits : int

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; range_start : 'a
    ; range_end : 'a
    ; range_valid : 'a
    ; ranges_done : 'a
    ; check_id : 'a
    ; check_valid : 'a
    ; checking_done : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { fresh_count : 'a
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
