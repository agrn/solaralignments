# Solar Alignments

This project allows one to find an location to see an alignment between the sun
and a target (e.g. a mountain).  It is available either as a command-line tool
(WIP), or as a web interface with a map.

## Acknowledgements

This project would be nothing without _Astronomical Algorithms_, 1st edition,
written by Jean Meeus.  It is available online if you know where to look.

This project is mostly written in OCaml, and relies on CalendarLib and
js_of_ocaml.

The map shown on the web interface is rendered with Leaflet, using data from
OpenStreetMap and tiles from OpenStreetMap-fr.

## License

Solar Alignments is licensed under LGPL v3.

## How to use

For now, only a `shell.nix` file is provided.

```sh
$ nix-shell
$ dune build --profile release
```

This will build the command-line tool and the web interface.

## Roadmap

In no particular order:
 - better README;
 - better command-line tool;
 - compute alignments from the sunrise, as well as arbitrary time ranges.
