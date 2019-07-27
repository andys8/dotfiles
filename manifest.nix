let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz";
    sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
  }) {};
in {

  inherit (pkgs)
    arandr # gui for monitors with xrandr
    autojump # jump into directory with j
    dtrx # extract files
    fzf # fuzzy find tool
    maim # screenshots
    nix # nix package manager
    ranger # file manager
    ripgrep # fast grep
    screenkey # visualize typed keys
  ;

  inherit (pkgs.elmPackages)
    elm # compiler
    elm-format # elm pretty print
    elm-test # elm test runner
  ;

  inherit (pkgs.gitAndTools)
    diff-so-fancy # git diff with colors
  ;

  inherit (fixed)
    cargo # rust package manager
    exa # ls replacement
    hlint # haskell linter
    ncdu # disk usage
    sbt # sbt package manager
    scala # scala compiler
    shfmt # format shell scripts
    wcalc # calculator
  ;
}
