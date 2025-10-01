(* (C) 2025 Alban Gruin, released under LGPL 3 *)

open CalendarLib

open Coordinates

let () =
  match Alignment.find (Date.today ()) 0.141277 42.937056 1.44 1.79 150000. with
  | None -> print_endline "No sunset today"
  | Some [] -> print_endline "No event today"
  | Some l ->
     Time_Zone.(change Local);
     List.iter
       (fun (dt, pos, (x, y, _)) ->
         Printf.printf "At %s, az=%f, alt=%f (pos at %f, %f)\n" (Printer.Calendar.to_string dt) pos.az pos.alt x y) l
