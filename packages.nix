let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz") {};
in
(
  with fixed; [
    arandr # gui for monitors with xrandr
    autorandr # xrandr monitor profiles
    awscli # amazon web services
    cabal2nix # Convert cabal files to nix
    cachix # nix chaching
    cargo # rust package manager
    chromium # web browser
    coursier # scala build tool
    ctop # top for containers
    exa # ls replacement
    ghcid # ghci daemon
    gnome3.zenity # show ui messages and dialogs
    hlint # haskell linter
    jq # json processor
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
    stalonetray # tray icons
    time # measure time
    trash-cli # rm into trash folder
    toilet # ascii art fonts
    vokoscreen # screen recorder
    wcalc # calculator
    xrandr-invert-colors # invert colors
    yarn # alternative node package manager
  ]
) ++ (
  with pkgs; [
    ack # search in files
    autojump # jump into directory with j
    cbatticon # battery icon and warning
    dtrx # extract files
    fd # "find" for files
    feh # Background viewer
    fzf # fuzzy find tool
    glow # markdown viewer
    i3status-rust # i3 status bar
    lazygit # git terminal ui
    maim # screenshots
    mdcat # markdown viewer
    meld # git merge tool (graphical)
    mpv # video player
    nix # nix package manager
    nixpkgs-fmt # format nix
    pa_applet # volume tray icon
    pavucontrol # pulse audio settings
    ranger # file manager
    rclone # copy cloud to cloud
    ripgrep # fast grep
    screenkey # visualize typed keys
    shellcheck # linter for shell scripts
    slack-term # slack tui
    sxiv # image viewer
    translate-shell # trans command
    xmobar # status bar for xmonad
    xmonad-with-packages # window manager in haskell
    zstd # compress tool used for .std
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
)
