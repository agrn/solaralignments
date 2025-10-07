(* (C) 2025 Alban Gruin, released under LGPL 3 *)

type ellipsoid

val wgs84 : ellipsoid

val direct : ellipsoid -> float -> float -> float -> float -> (float * float * float) option
val inverse : ellipsoid -> float -> float -> float -> float -> (float * float * float) option
