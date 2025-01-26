(* (C) 2025 Alban Gruin, released under LGPL 3 *)

open Float

open Helpers

type geodesic = {
    axis : float;
    flattening : float
  }

module Geodesic = struct
  type t = geodesic

  let make axis flattening = { axis; flattening }

  let wgs84 = make 6378137. (1. /. 298.25722356)
end

let direct geodesic latitude longitude bearing distance =
  let a, f = geodesic.axis, geodesic.flattening in
  let b = a *. (1. -. f) in

  let latitude = deg2rad latitude and
      longitude = deg2rad longitude and
      bearing = deg2rad bearing in

  let tanU = (1. -. f) *. tan latitude in
  let u = atan tanU in
  let sinU = sin u and
      cosU = cos u in

  let sinAlpha = cosU *. sin bearing in
  let cosAlpha = 1. -. sinAlpha *. sinAlpha in

  let uSquared = cosAlpha *. ((a *. a -. b *. b) /. (b *. b)) and
      sigma1 = atan2 tanU (cos bearing) in

  let bA = 1. +. (uSquared /. 16384.) *. (4096. +. uSquared *. (-768. +. uSquared *. (320. -. 175. *. uSquared))) and
      bB = (uSquared /. 1024.) *. (256. +. uSquared *. (-128. +. uSquared *. (74. -. 47. *. uSquared))) in

  let baseSigma = distance /. (b *. bA) in

  let rec converge delta sigma twoSigmaM cnt =
    if delta > 1.0e-9 && cnt < 50 then
      let twoSigmaM = 2. *. sigma1 +. sigma in
      let deltaSigma =
        bB *. sin sigma *.
          (cos twoSigmaM +. (bB /. 4.) *.
                              ((cos sigma) *. (-1. +. 2. *. (pow (cos twoSigmaM) 2.) -.
                                                 (bB /. 6.) *. (cos twoSigmaM) *.
                                                   (-3. +. 4. *. (pow (sin sigma) 2.)) *.
                                                     (-3. +. 4. *. (pow (cos twoSigmaM) 2.))))) in
      let sigma' = baseSigma +. deltaSigma in
      let delta = abs ((sigma' -. sigma) /. sigma') in
      converge delta sigma' twoSigmaM (cnt + 1)
    else if delta > 1.0e-9 then
      None
    else
      Some (sigma, twoSigmaM) in

  converge infinity baseSigma 0. 0
  |> Option.map (fun (sigma, twoSigmaM) ->
         let sinSigma = sin sigma and
             cosSigma = cos sigma in

         let phi2 =
           atan2 (sinU *. cosSigma +. cosU *. sinSigma *. (cos bearing))
             ((1. -. f) *. sqrt((sinAlpha ** 2.) +. (pow (sinU *. sinSigma -. cosU *. cosSigma *. (cos bearing)) 2.))) and

             lambda = atan2 (sinSigma *. (sin bearing)) (cosU *. cosSigma -. sinU *. sinSigma *. (cos bearing)) and
             c = (f /. 16.) *. cosAlpha *. (4. +. f *. (4. -. 3. *. cosAlpha)) in
         let l = lambda -. (1. -. c) *. f *. sinAlpha *. (sigma +. c *. sinSigma *. ((cos twoSigmaM) +. c *. cosSigma *. (-1. +. 2. *. (pow (cos twoSigmaM) 2.)))) and
             alpha2 = atan2 sinAlpha (-. sinU *. sinSigma +. cosU *. cosSigma *. (cos bearing)) in

         rad2deg phi2, rad2deg (longitude +. l), rad2deg alpha2)
