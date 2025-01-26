(* (C) 2025 Alban Gruin, released under LGPL 3 *)

module Geodesic : sig
  type t

  val make : float -> float -> t
  val wgs84 : t
end

val direct : Geodesic.t -> float -> float -> float -> float -> (float * float * float) option
