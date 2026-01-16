open! Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; num_cols : 'a
    ; start_col : 'a
    ; set_dims : 'a
    ; splitter_row : 'a
    ; splitter_col : 'a
    ; splitter_valid : 'a
    ; finish_load : 'a
    ; reachable_count : 'a
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
