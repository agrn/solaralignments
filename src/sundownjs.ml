(* (C) 2025 Alban Gruin, released under LGPL 3 *)

open CalendarLib
open Js_of_ocaml

type preset = {
    name : string;
    latitude : float;
    longitude : float;
    target_altitude : float;
    observer_altitude : float;
    distance : float
  }

let presets = [
    {name = "Pic du Midi de Bigorre";
     longitude = 0.141277; latitude = 42.937056; target_altitude = 2877.;
     observer_altitude = 200.; distance = 150.}
  ]

let (let*) = Option.bind

let get_field id = Dom_html.(getElementById_coerce id CoerceTo.input)

let update_pin (lat : float) (lng : float) =
  ignore @@ Js.Unsafe.fun_call (Js.Unsafe.js_expr "updatePin") [|Js.Unsafe.inject lat; Js.Unsafe.inject lng|]

let read_float_from_field field =
  Js.(to_float @@ parseFloat field##.value)

let write_value value field =
  let num = Js.number_of_float value in
  field##.value := num##toString

let update_altitude altitude_field observer_field distance_field lowest_field highest_field =
  let ho = read_float_from_field observer_field and
      hm = read_float_from_field altitude_field and
      dist = 1000. *. read_float_from_field distance_field in
  let altitude = Apparent.apparent_height ho hm dist 6371000. in
  let avg_sun_size = 0.52 in
  write_value (altitude -. (avg_sun_size /. 2.)) lowest_field;
  write_value (altitude +. (avg_sun_size /. 2.)) highest_field

let setup_presets (preset_field : Dom_html.selectElement Js.t) longitude_field latitude_field altitude_field observer_field lowest_field highest_field distance_field =
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
              let* preset = List.nth_opt presets idx in
              write_value preset.longitude longitude_field;
              write_value preset.latitude latitude_field;
              write_value preset.target_altitude altitude_field;
              write_value preset.observer_altitude observer_field;
              write_value preset.distance distance_field;
              update_pin preset.latitude preset.longitude;
              update_altitude altitude_field observer_field distance_field lowest_field highest_field;
              None
          end;
        Js._true)

let setup_today today_button date_field =
  today_button##.onclick :=
    Dom_html.handler (fun evt ->
        Dom.preventDefault evt;
        let today = Printer.Date.to_string @@ Date.today () in
        date_field##.value := Js.string today;
        Js._true)

let reset_preset_index preset_field =
  preset_field##.selectedIndex := 0

let read_and_update_pin preset_field longitude_field latitude_field _evt =
  reset_preset_index preset_field;
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
     List.map (fun (dt, pos, coords) ->
         Calendar.(new%js Js.date_min (year dt) (Date.int_of_month (month dt)) (day_of_month dt) (hour dt) (minute dt)),
         pos, coords) l
     |> Array.of_list
     |> Js.array
     |> Js.Optdef.return

let () =
  ignore @@
    let* preset_field = Dom_html.(getElementById_coerce "preset" CoerceTo.select) in
    let* today_button = Dom_html.(getElementById_coerce "today" CoerceTo.button) in
    let* date_field = get_field "date" in
    let* longitude_field = get_field "longitude" in
    let* latitude_field = get_field "latitude" in
    let* altitude_field = get_field "altitude" in
    let* observer_field = get_field "observer" in
    let* lowest_field = get_field "lowest" in
    let* highest_field = get_field "highest" in
    let* distance_field = get_field "distance" in
    let* pin_check = get_field "pin" in
    setup_presets preset_field longitude_field latitude_field altitude_field observer_field lowest_field highest_field distance_field;
    setup_today today_button date_field;
    let update_altitude _evt =
      update_altitude altitude_field observer_field distance_field lowest_field highest_field;
      Js._true in
    longitude_field##.onchange := Dom_html.handler (read_and_update_pin preset_field longitude_field latitude_field);
    latitude_field##.onchange := Dom_html.handler (read_and_update_pin preset_field longitude_field latitude_field);
    altitude_field##.onchange := Dom_html.handler (fun _evt -> reset_preset_index preset_field; update_altitude ());
    observer_field##.onchange := Dom_html.handler update_altitude;
    distance_field##.onchange := Dom_html.handler update_altitude;
    Js.export "alignment"
      (object%js
         method find = alignment_find
         method handleMapClickEvent = handle_map_click_event pin_check preset_field longitude_field latitude_field
       end);
    None
