(* (C) 2025 Alban Gruin, released under LGPL 3 *)

open CalendarLib
open Js_of_ocaml

type preset = {
    name : string;
    latitude : float;
    longitude : float;
    lowest : float;
    highest : float;
    distance : float
  }

let presets = [
    {name = "Pic du Midi de Bigorre from Toulouse";
     longitude = 0.141277; latitude = 42.937056; lowest = 1.44; highest = 1.79; distance = 150000.}
  ]

let (>>=) = Option.bind

let get_field id = Dom_html.(getElementById_coerce id CoerceTo.input)

let setup_presets (preset_field : Dom_html.selectElement Js.t) longitude_field latitude_field lowest_field highest_field distance_field =
  let write_value value field =
    let num = Js.number_of_float value in
    field##.value := num##toString in
  let document = Dom_html.document in
  List.iter (fun preset ->
      let option = Dom_html.createOption document in
      Dom.appendChild option (document##createTextNode (Js.string preset.name));
      preset_field##add option Js.null) presets;
  preset_field##.onchange :=
    Dom_html.handler (fun _ ->
        let idx = preset_field##.selectedIndex - 1 in
        if idx >= 0 then begin
            ignore @@
              (List.nth_opt presets idx >>= fun preset ->
               write_value preset.longitude longitude_field;
               write_value preset.latitude latitude_field;
               write_value preset.lowest lowest_field;
               write_value preset.highest highest_field;
               write_value preset.distance distance_field;
               None)
          end;
        Js._true)

let alignment_find date latitude longitude lowest highest distance =
  let date = Date.make (date##getFullYear) (date##getMonth + 1) (date##getDate) in
  match Alignment.find date longitude latitude lowest highest distance with
  | None -> Js.Optdef.empty
  | Some l ->
     begin
       Time_Zone.(change Local);
       let result = List.map (fun (dt, pos, coords) ->
                        Calendar.(new%js Js.date_min (year dt) (Date.int_of_month (month dt)) (day_of_month dt) (hour dt) (minute dt)),
                        pos, Js.Optdef.option coords) l
                    |> Array.of_list
                    |> Js.array
                    |> Js.Optdef.return in
       Time_Zone.(change UTC);
       result
     end

let () =
  Js.export "alignment"
    (object%js
       method find = alignment_find
     end);
  ignore @@
    (Dom_html.(getElementById_coerce "preset" CoerceTo.select) >>= fun preset_field ->
     get_field "longitude" >>= fun longitude_field ->
     get_field "latitude" >>= fun latitude_field ->
     get_field "lowest" >>= fun lowest_field ->
     get_field "highest" >>= fun highest_field ->
     get_field "distance" >>= fun distance_field ->
     Some (setup_presets preset_field longitude_field latitude_field lowest_field highest_field distance_field))
