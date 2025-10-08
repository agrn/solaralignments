(* (C) 2025 Alban Gruin, released under LGPL 3 *)

val deg2rad : float -> float
val rad2deg : float -> float

val dsin : float -> float
val dcos : float -> float
val dtan : float -> float

val (%.) : float -> float -> float

val subdeg : float -> float -> float -> float

val interpolate : float -> float -> float -> float -> float
