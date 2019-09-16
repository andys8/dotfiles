let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz") {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  elmTools = import (pkgs.fetchFromGitHub {
    owner = "andys8";
    repo = "nix-elm-tools";
    rev = "fd9773caff6da2d62b1b792346c151fd23bca50e";
    sha256 = "1m8j4m3m3v78a69jc2id7l6n64w2h4f4lgvi54c319irqk8zbrlr";
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
  rofi # dmenu replacement
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
  diff-so-fancy # git diff with colors
  git-gone # prune merged branches
  hub # github commandline tool
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
    inherit (p) ghc864 ghc865 ghc843; # haskell ide engine
  };
})]
