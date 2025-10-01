(* (C) 2025 Alban Gruin, released under LGPL 3 *)

open CalendarLib

open Coordinates
open Helpers

let j2000 jd =
  (* Astronomical Algorithms, Jean Meeus, Formule 24.1 *)
  (jd -. 2451545.) /. 36525.

let obliquity t =
  (* Astronomical Algorithms, Jean Meeus, Formule 21.2 *)
  (subdeg 23. 26. 21.448) +.
    t *. (-.(subdeg 0. 0. 46.8150) +.
            t *. (-.(subdeg 0. 0. 0.00059) +.
                    t *. subdeg 0. 0. 0.001813))

let nutation dt =
  (* Astronomical Algorithms, Jean Meeus, Chapitre 21, méthode basse fidélité *)
  let jd = Calendar.to_jd dt in
  let t = j2000 jd in

  let omega = 125.04452 -. 1934.136261 *. t in

  let lsun = 280.4665 +. 36000.7698 *. t in
  let lmoon = 218.3165 +. 481267.8813 *. t in

  let delta_psi = -. (subdeg 0. 0. 17.2) *. (dsin omega) -. (subdeg 0. 0. 1.32) *. (dsin 2. *. lsun) -.
                    (subdeg 0. 0. 0.23) *. (dsin 2. *. lmoon) +. (subdeg 0. 0. 0.21) *. (dsin 2. *. omega) in
  let delta_epsilon = (subdeg 0. 0. 9.20) *. (dcos omega) +. (subdeg 0. 0. 0.57) *. (dcos 2. *. lsun) +.
                        (subdeg 0. 0. 0.1) *. (dcos 2. *. lmoon) -. (subdeg 0. 0. 0.09) *. (dcos 2. *. omega) in

  let epsilon0 = obliquity t in
  let epsilon = epsilon0 +. delta_epsilon in

  delta_psi, epsilon

let sidereal_time ?(add_nutation=false) dt =
  (* Astronomical Algorithms, Jean Meeus, Chapitre 11 *)
  let jd = Calendar.to_jd dt in
  let t = j2000 jd in
  let delta_psi, epsilon =
    if add_nutation then
      nutation dt
    else
      0., 0. in
  let theta0 =
    (280.46061837 +. 360.98564736629 *. (jd -. 2451545.) +.
       0.000387933 *. (t ** 2.) -. ((t ** 3.) /. 38710000.)) (* 11.4 *)
      (* Add nutation *) +. delta_psi *. dcos epsilon in
  theta0 %. 360.

let local_hour sidereal longitude ra =
  (* Astronomical Algorithms, Jean Meeus, Chapitre 12 *)
  sidereal -. longitude -. ra

let alt lh latitude coords =
  (* Astronomical Algorithms, Jean Meeus, Formule 12.6 *)
  Float.asin ((dsin latitude) *. (dsin coords.dec) +. (dcos latitude) *. (dcos coords.dec) *. (dcos lh)) (* 12.6 *)

let horizontal_of_equatorial ?(add_nutation=false) dt longitude latitude coords =
  (* Astronomical Algorithms, Jean Meeus, Formules 12.5 et 12.6 *)
  let sidereal = sidereal_time ~add_nutation dt in
  let lh = local_hour sidereal longitude coords.ra in

  (* 12.5 *)
  let az = Float.atan2
             (dsin lh)
             ((dcos lh) *. (dsin latitude) -. (dtan coords.dec) *. (dcos latitude)) +.
             Float.pi in (* 0° at the North instead of the South *)
  let alt = alt lh latitude coords in
  { alt = rad2deg alt; az = rad2deg az }

let solar jd =
  (* Calcul des coordonnées solaires, basé sur Astronomical Algorithms, Jean
     Meeus, Chapitre 24, méthode basse fidélité *)
  let t = j2000 jd in
  let l0 = (280.46645 +. t *. 36000.76983 +. (t *. t) *. 0.0003032) %. 360. in (* 24.2 *)
  let m = (357.52910 +. t *. (35999.05030 +. t *. (-0.0001559 -. t *. 0.00000048))) %. 360. in (* 24.3 *)

  let c = (1.914600 -. t *. (0.004817 +. 0.000014 *. t)) *. (dsin m) +.
            (0.019993 -. 0.000101 *. t) *. (dsin (2. *. m)) +.
            0.000290 *. (dsin (3. *. m)) in

  let theta = l0 +. c in

  let omega = 125.04 -. 1934.136 *. t in
  let lm = theta -. 0.00569 -. 0.00478 *. (dsin omega) in

  let epsilon0 = obliquity t in
  let epsilon = epsilon0 +. 0.00256 *. (dcos omega) in (* 24.8 *)

  let ra = Float.atan2 ((dcos epsilon) *. (dsin lm)) (dcos lm) in (* 24.6 *)
  let dec = Float.asin ((dsin epsilon) *. (dsin lm)) in (* 24.7 *)

  { ra = (rad2deg ra) %. 360.; dec = (rad2deg dec) %. 360. }

let sunset ?(add_nutation=false) date longitude latitude =
  (* Calcul de l'heure du coucher du soleil, basé sur Astronomical Algorithms, Jean Meeus, Chapitre 14 *)
  let posd = Calendar.(solar @@ to_jd @@ from_date date) in

  let h0 = -0.8333 in
  let theta0 = sidereal_time ~add_nutation (Calendar.from_date date) in
  let div = (dcos latitude) *. (dcos (posd.dec)) in
  if Float.abs div <= 1. then
    let h0 = rad2deg (Float.acos (((dsin h0) -. (dsin latitude) *. (dsin posd.dec)) /. div)) %. 180. in (* 14.1 *)

    (* 14.2 *)
    let m0 = ((posd.ra +. longitude -. theta0) /. 360.) %. 1. in
    let m2 = ((m0 +. h0 /. 360.) %. 1.) *. 24. in
    let time = Time.from_hours m2 in
    Some (Calendar.create date time)
  else
    None

let find day longitude latitude lowest highest distance =
  sunset day (-. longitude) latitude
  |> Option.map (fun dt ->
         let sunset = Calendar.(rem dt (Period.second (second dt))) in

         let position =
           let next dt =
             Calendar.(rem dt (Period.minute 1)) in
           Seq.iterate next sunset
           |> Seq.map (fun dt ->
                  dt,
                  solar @@ Calendar.to_jd dt
                  |> horizontal_of_equatorial dt (-. longitude) latitude)
           |> Seq.take_while (fun (dt, pos) ->
                  Date.equal day (Calendar.to_date dt) && pos.alt < highest)
           |> Seq.drop_while (fun (_, pos) -> pos.alt < lowest)
           |> Seq.map (fun (dt, pos) ->
                let bearing = (pos.az +. 180.) %. 360. in
                dt, pos, Vincenty.(direct Geodesic.wgs84 latitude longitude bearing distance))
           |> List.of_seq in

         position)
