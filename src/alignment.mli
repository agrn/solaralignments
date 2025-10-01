(* (C) 2025 Alban Gruin, released under LGPL 3 *)

val find : CalendarLib.Date.t -> float -> float -> float -> float -> float ->
           (CalendarLib.Calendar.t * Coordinates.horizontal * (float * float * float)) list option
