open! Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; num_nodes : 'a
    ; set_num_nodes : 'a
    ; edge_a : 'a
    ; edge_b : 'a
    ; edge_x1 : 'a
    ; edge_x2 : 'a
    ; edge_valid : 'a
    ; finish : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { part1 : 'a
    ; part2 : 'a
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
