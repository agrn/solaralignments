(* (C) 2025 Alban Gruin, released under LGPL 3 *)

open Float

open Helpers

type ellipsoid = {
    axis : float;
    flattening : float
  }

let wgs84 = { axis = 6378137.; flattening = (1. /. 298.25722356) }

let getSmallB ellipsoid =
  ellipsoid.axis *. (1. -. ellipsoid.flattening)

(* Implementation of Vincenty'd direct and inverse formulae. *)
(* https://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf *)

let getABC ellipsoid cosAlpha =
  let b = getSmallB ellipsoid in
  let uSquared = cosAlpha *. ((ellipsoid.axis *. ellipsoid.axis -. b *. b) /. (b *. b)) in

  let a = 1. +. (uSquared /. 16384.) *. (4096. +. uSquared *. (-768. +. uSquared *. (320. -. 175. *. uSquared))) and
      b = (uSquared /. 1024.) *. (256. +. uSquared *. (-128. +. uSquared *. (74. -. 47. *. uSquared))) and
      c = (ellipsoid.flattening /. 16.) *. cosAlpha *. (4. +. ellipsoid.flattening *. (4. -. 3. *. cosAlpha)) in
  a, b, c

let reduced_latitude ellipsoid latitude =
  let tanU = (1. -. ellipsoid.flattening) *. tan latitude in
  tanU, atan tanU

let latitude_remainder c f sinAlpha sigma sinSigma cosSigma cos2sigmaM =
  (1. -. c) *. f *. sinAlpha *. (sigma +. c *. sinSigma *. ((cos2sigmaM) +. c *. cosSigma *. (-1. +. 2. *. (pow (cos2sigmaM) 2.))))

let delta_sigma bB sigma cos2sigmaM =
  bB *. sin sigma *.
    (cos2sigmaM +. (bB /. 4.) *.
                     ((cos sigma) *. (-1. +. 2. *. (pow (cos2sigmaM) 2.) -.
                                        (bB /. 6.) *. (cos2sigmaM) *.
                                          (-3. +. 4. *. (pow (sin sigma) 2.)) *.
                                            (-3. +. 4. *. (pow (cos2sigmaM) 2.)))))

let direct ellipsoid latitude longitude bearing distance =
  let f = ellipsoid.flattening in
  let b = getSmallB ellipsoid in

  let latitude = deg2rad latitude and
      longitude = deg2rad longitude and
      bearing = deg2rad bearing in

  let tanU, u = reduced_latitude ellipsoid latitude in
  let sinU = sin u and
      cosU = cos u in

  let sinAlpha = cosU *. sin bearing in
  let cosAlpha = 1. -. sinAlpha *. sinAlpha in

  let sigma1 = atan2 tanU (cos bearing) in

  let bA, bB, c = getABC ellipsoid cosAlpha in

  let baseSigma = distance /. (b *. bA) in

  let rec converge delta sigma twoSigmaM cnt =
    if delta > 1.0e-9 && cnt < 50 then
      let twoSigmaM = 2. *. sigma1 +. sigma in
      let deltaSigma = delta_sigma bB sigma (cos twoSigmaM) in
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
             ((1. -. f) *. sqrt ((sinAlpha ** 2.) +. (pow (sinU *. sinSigma -. cosU *. cosSigma *. (cos bearing)) 2.))) and

             lambda = atan2 (sinSigma *. (sin bearing)) (cosU *. cosSigma -. sinU *. sinSigma *. (cos bearing)) in
         let l = lambda -. latitude_remainder c f sinAlpha sigma sinSigma cosSigma (cos twoSigmaM) and
             alpha2 = atan2 sinAlpha (-. sinU *. sinSigma +. cosU *. cosSigma *. (cos bearing)) in

         rad2deg phi2, rad2deg (longitude +. l), rad2deg alpha2)

let inverse ellipsoid lat1 long1 lat2 long2 =
  let f = ellipsoid.flattening in
  let b = getSmallB ellipsoid in

  let lat1 = deg2rad lat1 and
      long1 = deg2rad long1 and
      lat2 = deg2rad lat2 and
      long2 = deg2rad long2 in

  let l = long2 -. long1 in
  let _, u1 = reduced_latitude ellipsoid lat1 and
      _, u2 = reduced_latitude ellipsoid lat2 in

  let rec converge delta lambda para cnt =
    if delta > 1.0e-12 && cnt < 50 then
      let sinSigma = pow (cos u2 *. sin lambda) 2. +.
                       pow (cos u1 *. sin u2 -. sin u1 *. cos u2 *. cos lambda) 2. |> sqrt in
      let cosSigma = sin u1 *. sin u2 +. cos u1 *. cos u2 *. cos lambda in
      let sigma = atan2 sinSigma cosSigma in
      let sinAlpha = (cos u1 *. cos u2 *. sin lambda) /. sinSigma in
      let cos2alpha = 1. -. sinAlpha *. sinAlpha in
      let cos2sigmaM = cosSigma -. (2. *. sin u1 *. sin u2) /. cos2alpha in
      let bA, bB, c = getABC ellipsoid cos2alpha in
      let lambda' = l +. latitude_remainder c f sinAlpha sigma sinSigma cosSigma cos2sigmaM in
      let delta = abs (lambda' -. lambda) in
      converge delta lambda' (bA, bB, sigma, cos2sigmaM) (cnt + 1)
    else if delta > 1.0e-12 then
      None
    else
      Some (lambda, para)  in

  converge infinity l (0., 0., 0., 0.) 0
  |> Option.map (fun (lambda, (bA, bB, sigma, cos2sigmaM)) ->
         let deltaSigma = delta_sigma bB sigma cos2sigmaM in
         let s = b *. bA *. (sigma -. deltaSigma) in
         let alpha1 = atan2 (cos u2 *. sin lambda) (cos u1 *. sin u2 -. sin u1 *. cos u2 *. cos lambda) in
         let alpha2 = atan2 (cos u1 *. sin lambda) (cos u1 *. sin u2 *. cos lambda -. sin u1 *. cos u2) in
         s, rad2deg alpha1, rad2deg alpha2)
