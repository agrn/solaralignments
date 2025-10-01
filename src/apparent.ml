(* (C) 2025 Alban Gruin, released under LGPL 3 *)

open Helpers

let bennett_refr h =
  (* Jean Meeus, Astronomical Algorithms, Formule 15.4 *)
  h +. subdeg 0. (1.02 /. (dtan (h +. (10.3 /. (h +. 5.11))))) 0.
