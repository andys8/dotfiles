let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz";
    sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
  }) {};
in {
  inherit (pkgs) nix;
  inherit (pkgs.elmPackages) elm elm-format elm-test;
  inherit (fixed) hello cargo scala sbt;
}
