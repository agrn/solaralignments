(* (C) 2025 Alban Gruin, released under LGPL 3 *)

let deg2rad d =
  (d /. 180.) *. Float.pi

let rad2deg r =
  (r /. Float.pi) *. 180.

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

let interpolate n y1 y2 y3 =
  (* Interpolation, from Astronomical Algorithms, Jean Meeus, Chapter 3 *)
  let a = y2 -. y1 and
      b = y3 -. y2 in
  let c = b -. a in
  y2 +. (n /. 2.) *. (a +. b +. n *. c) (* 3.3 *)
