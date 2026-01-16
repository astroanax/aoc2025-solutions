open! Hardcaml

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
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
