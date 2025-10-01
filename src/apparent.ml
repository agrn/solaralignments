(* (C) 2025 Alban Gruin, released under LGPL 3 *)

open Float

open Helpers

(* https://aty.sdsu.edu/explain/atmos_refr/altitudes.html *)
let apparent_height ho hm dist r =
  let k = 35. /. 150. in
  let h = (dist *. dist) *. (1. -. k) /. (2. *. r) in
  let h' = ho +. h in
  let altrad = atan2 (hm -. h') dist in
  rad2deg altrad

let bennett_refr h =
  (* Jean Meeus, Astronomical Algorithms, Formule 15.4 *)
  h +. subdeg 0. (1.02 /. (dtan (h +. (10.3 /. (h +. 5.11))))) 0.
