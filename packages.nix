let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz") {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
(
  with fixed; [
    arandr # gui for monitors with xrandr
    autorandr # xrandr monitor profiles
    cabal-install # haskell build tool
    cachix # cache nix packages
    cargo # rust package manager
    coursier # scala build tool
    exa # ls replacement
    ghc # haskell compiler
    hlint # haskell linter
    mdp # markdown presentation
    ncdu # disk usage
    nethogs # network traffic monitoring
    nodejs-12_x # nodejs and npm
    rofi # dmenu replacement
    sbt # sbt package manager
    scala # scala compiler
    sent # suckless presentation tool
    shfmt # format shell scripts
    toilet # ascii art fonts
    vokoscreen # screen recorder
    wcalc # calculator
  ]
) ++ (
  with pkgs; [
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
    nixpkgs-fmt # format nix
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
  ]
) ++ (
  with pkgs.gitAndTools; [
    diff-so-fancy # git diff with colors
    git-gone # prune merged branches
    hub # github commandline tool
    tig # diff and comit view
  ]
) ++ (
  with pkgs.elmPackages; [
    elm # compiler
    elm-format # elm pretty print
    elm-language-server # language-server
    elm-test # elm test runner
  ]
) ++ (
  with pkgs.haskellPackages; [
    brittany # code formatter
    git-brunch # git checkout branch tui
    hindent # Haskell format (alternative)
    hoogle # documenation
    network-manager-tui # network tui
  ]
) ++ [
  (
    all-hies.selection {
      selector = p: {
        inherit (p) ghc864 ghc865 ghc843; # haskell ide engine
      };
    }
  )
]
