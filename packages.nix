let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz") {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  elmTools = import (pkgs.fetchFromGitHub {
    owner = "turboMaCk";
    repo = "nix-elm-tools";
    rev = "d138d5405a5df4903c753079a649b70ab28cc8d8";
    sha256 = "1n3kybxnm93jjsaww7pa7bgm8xal81c5sf8aw7ag6r5jjw16skm8";
  }) { inherit pkgs; };
in
(with fixed; [
  cabal-install # haskell build tool
  cachix # cache nix packages
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
  sent # suckless presentation tool
]) ++
(with pkgs; [
  arandr # gui for monitors with xrandr
  autojump # jump into directory with j
  autorandr # xrandr monitor profiles
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
  pa_applet # volume tray icon
  pass # password manager
  pavucontrol # pulse audio settings
  ranger # file manager
  ripgrep # fast grep
  screenkey # visualize typed keys
  shellcheck # linter for shell scripts
  sxiv # image viewer
  translate-shell # trans command
  trayer # tray icons for xmonad
  xmobar # status bar for xmonad
  xmonad-with-packages # window manager in haskell
]) ++
(with pkgs.gitAndTools; [
  git-gone # prune merged branches
  diff-so-fancy # git diff with colors
  tig # diff and comit view
]) ++
(with pkgs.elmPackages; [
  elm # compiler
  elm-format # elm pretty print
  elm-test # elm test runner
]) ++
(with elmTools; [
  elm-language-server
]) ++
(with pkgs.haskellPackages; [
  brittany # code formatter
  git-brunch # git checkout branch tui
  hoogle # documenation
  hindent # Haskell format (alternative)
]) ++
[(all-hies.selection {
  selector = p: {
    inherit (p) ghc864 ghc865; # haskell ide engine
  };
})]
