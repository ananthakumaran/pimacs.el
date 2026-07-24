{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = [ pkgs.nodejs_22 pkgs.emacs.pkgs.cask pkgs.pandoc ];
}
