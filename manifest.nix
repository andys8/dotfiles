let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz";
    sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
  }) {};
in {

  inherit (pkgs)
    nix
    fzf # fuzzy find tool
  ;

  inherit (pkgs.elmPackages)
    elm # compiler
    elm-format
    elm-test # elm test runner
  ;

  inherit (pkgs.gitAndTools)
    diff-so-fancy # git diff with colors
  ;

  inherit (fixed)
    cargo # rust package manager
    scala # scala compiler
    sbt # sbt package manager
    exa # ls replacement
    shfmt # format shell scripts
    hlint # haskell linter
    mdp # markdown presentations
  ;
}
