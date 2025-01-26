{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    dune_3
    ocamlPackages.calendar
    ocamlPackages.findlib
    ocamlPackages.js_of_ocaml
    ocamlPackages.js_of_ocaml-compiler
    ocamlPackages.js_of_ocaml-ppx
    ocamlPackages.utop
  ];
}
