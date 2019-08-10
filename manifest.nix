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
    fd # "find" for files
    feh # Background viewer
    fzf # fuzzy find tool
    gopass # password manager for pass
    i3status-rust # i3 status bar
    lazygit # git terminal ui
    maim # screenshots
    meld # git merge tool (graphical)
    nix # nix package manager
    pass # password manager
    ranger # file manager
    ripgrep # fast grep
    screenkey # visualize typed keys
    shellcheck # linter for shell scripts
    sxiv # image viewer
  ;

  inherit (pkgs.elmPackages)
    elm # compiler
    elm-format # elm pretty print
    elm-test # elm test runner
  ;

  inherit (pkgs.haskellPackages)
    brittany # code formatter
  ;

  inherit (pkgs.gitAndTools)
    git-gone # prune merged branches
    diff-so-fancy # git diff with colors
    tig # diff and comit view
  ;

  inherit (fixed)
    cabal-install # haskell build tool
    cargo # rust package manager
    exa # ls replacement
    ghc # haskell compiler
    hlint # haskell linter
    ncdu # disk usage
    nodejs-11_x # nodejs and npm
    sbt # sbt package manager
    scala # scala compiler
    shfmt # format shell scripts
    vokoscreen # screen recorder
    wcalc # calculator
  ;
}
