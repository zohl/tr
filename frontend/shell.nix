{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;

  version = "0.0.1";

  nodePackages = pkgs.nodePackages; 

in nixpkgs.stdenv.mkDerivation {
    name = "tr-frontend-${version}";

    buildInputs = with pkgs.nodePackages; [
      webpack
    ];
    
    src = ./src;
  }

