(* interface for gift_shop module

   the hardware receives precomputed intersection parameters:
   - lower: first valid ID in the intersection of pattern and input range
   - upper: last valid ID in the intersection
   - step: step of the arithmetic sequence
   - n: count of IDs in the intersection, i.e. (upper-lower)/step

   for each input pulse, it computes the sum of the arithmetic sequence
   and accumulates it. when finish is signaled, result_valid goes high. *)

open! Hardcaml

val data_bits : int

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; lower : 'a
    ; upper : 'a
    ; step : 'a
    ; n : 'a
    ; input_valid : 'a
    ; finish : 'a
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

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
