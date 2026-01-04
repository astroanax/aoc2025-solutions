(* interface for roll_counter module *)

open! Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; rows : 'a
    ; cols : 'a
    ; set_dims : 'a
    ; cell_row : 'a
    ; cell_col : 'a
    ; cell_is_roll : 'a
    ; cell_neighbor_count : 'a
    ; cell_valid : 'a
    ; finish_load : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { part1 : 'a
    ; part2 : 'a
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
