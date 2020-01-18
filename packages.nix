let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz") {};
  # haskell-ide-engine 0.14.0.0
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/c6e93d2d641ef48703eabed8ec5cde3d774cb0e5") {};
in
(
  with fixed; [
    arandr # gui for monitors with xrandr
    autorandr # xrandr monitor profiles
    awscli # amazon web services
    cachix # nix chaching
    cabal2nix # Convert cabal files to nix
    cargo # rust package manager
    coursier # scala build tool
    ctop # top for containers
    exa # ls replacement
    ghcid # ghci daemon
    hlint # haskell linter
    mdp # markdown presentation
    ncdu # disk usage
    nethogs # network traffic monitoring
    nodejs-12_x # nodejs and npm
    rofi # dmenu replacement
    safe-rm # rm with safety
    sbt # sbt package manager
    scala # scala compiler
    sent # suckless presentation tool
    shfmt # format shell scripts
    toilet # ascii art fonts
    vokoscreen # screen recorder
    wcalc # calculator
    xrandr-invert-colors # invert colors
    yarn # alternative node package manager
    gnome3.zenity # show ui messages and dialogs
  ]
) ++ (
  with pkgs; [
    ack # search in files
    autojump # jump into directory with j
    rclone # copy cloud to cloud
    chromium # web browser
    dtrx # extract files
    fd # "find" for files
    feh # Background viewer
    fzf # fuzzy find tool
    i3status-rust # i3 status bar
    lazygit # git terminal ui
    maim # screenshots
    mdcat # markdown viewer
    meld # git merge tool (graphical)
    nix # nix package manager
    nixpkgs-fmt # format nix
    pa_applet # volume tray icon
    pavucontrol # pulse audio settings
    ranger # file manager
    ripgrep # fast grep
    screenkey # visualize typed keys
    shellcheck # linter for shell scripts
    slack-term # slack tui
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
    hoogle # function documentation
    network-manager-tui # network tui
  ]
) ++ [
  (
    # haskell ide engine
    all-hies.selection {
      selector = p: {
        inherit (p) ghc865;
      };
    }
  )
]
