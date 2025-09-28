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

let update_pin (lat : float) (lng : float) =
  ignore @@ Js.Unsafe.fun_call (Js.Unsafe.js_expr "updatePin") [|Js.Unsafe.inject lat; Js.Unsafe.inject lng|]

let write_value value field =
  let num = Js.number_of_float value in
  field##.value := num##toString

let setup_presets (preset_field : Dom_html.selectElement Js.t) longitude_field latitude_field lowest_field highest_field distance_field =
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
               update_pin preset.latitude preset.longitude;
               None)
          end;
        Js._true)

let setup_today today_button date_field =
  today_button##.onclick :=
    Dom_html.handler (fun evt ->
        Dom.preventDefault evt;
        let today = Printer.Date.to_string @@ Date.today () in
        date_field##.value := Js.string today;
        Js._true)

let reset_preset_index preset_field _evt =
  preset_field##.selectedIndex := 0;
  Js._true

let read_and_update_pin preset_field longitude_field latitude_field evt =
  ignore @@ reset_preset_index preset_field evt;
  let read_float_from_field field =
    Js.(to_float @@ parseFloat field##.value) in
  let lat = read_float_from_field latitude_field and
      lng = read_float_from_field longitude_field in
  update_pin lat lng;
  Js._true

let handle_map_click_event pin_check preset_field longitude_field latitude_field lat lng =
  if Js.to_bool pin_check##.checked then begin
      write_value lat latitude_field;
      write_value lng longitude_field;
      preset_field##.selectedIndex := 0;
      update_pin lat lng;
      pin_check##.checked := Js._false
    end

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
  ignore @@
    (Dom_html.(getElementById_coerce "preset" CoerceTo.select) >>= fun preset_field ->
     Dom_html.(getElementById_coerce "today" CoerceTo.button) >>= fun today_button ->
     get_field "date" >>= fun date_field ->
     get_field "longitude" >>= fun longitude_field ->
     get_field "latitude" >>= fun latitude_field ->
     get_field "lowest" >>= fun lowest_field ->
     get_field "highest" >>= fun highest_field ->
     get_field "distance" >>= fun distance_field ->
     get_field "pin" >>= fun pin_check ->
     setup_presets preset_field longitude_field latitude_field lowest_field highest_field distance_field;
     setup_today today_button date_field;
     longitude_field##.onchange := Dom_html.handler (read_and_update_pin preset_field longitude_field latitude_field);
     latitude_field##.onchange := Dom_html.handler (read_and_update_pin preset_field longitude_field latitude_field);
     lowest_field##.onchange := Dom_html.handler (reset_preset_index preset_field);
     highest_field##.onchange := Dom_html.handler (reset_preset_index preset_field);
     distance_field##.onchange := Dom_html.handler (reset_preset_index preset_field);
     Js.export "alignment"
       (object%js
          method find = alignment_find
          method handleMapClickEvent = handle_map_click_event pin_check preset_field longitude_field latitude_field
        end);
     None)
