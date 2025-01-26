(* (C) 2025 Alban Gruin, released under LGPL 3 *)

let deg2rad d =
  (d /. 360.) *. (2. *. Float.pi)

let rad2deg r =
  (r /. (2. *. Float.pi)) *. 360.

let dtrigo trig d = deg2rad d |> trig
let dsin = dtrigo Float.sin
let dcos = dtrigo Float.cos
let dtan = dtrigo Float.tan

let (%.) a b =
  let res = mod_float a b in
  if res < 0. then
    res +. b
  else
    res

let subdeg deg minute second =
  (deg +. (minute /. 60.) +. (second /. 3600.)) %. 360.
