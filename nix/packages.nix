let
  unstable = import <nixpkgs> {};
  release2003 = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") {};
  st = import ./st.nix;
in
(
  with release2003; [
    arandr # gui for monitors with xrandr
    autorandr # xrandr monitor profiles
    awscli # amazon web services
    cabal2nix # Convert cabal files to nix
    chromium # web browser
    coursier # scala build tool
    gnome3.zenity # show ui messages and dialogs
    i3status-rust # i3 status bar
    jq # json processor
    mdp # markdown presentation
    ncdu # disk usage
    nethogs # network traffic monitoring
    nitrogen # Wallpaper
    nix # nix package manager
    nodejs-12_x # nodejs and npm
    rofi # dmenu replacement
    sbt # sbt package manager
    scala # scala compiler
    sent # suckless presentation tool
    shfmt # format shell scripts
    stalonetray # tray icons
    time # measure time
    toilet # ascii art fonts
    trash-cli # rm into trash folder
    unzip # extract zip
    vokoscreen # screen recorder
    watch # repeat command
    wcalc # calculator
    xmobar # status bar for xmonad
    xmonad-with-packages # window manager in haskell
    xrandr-invert-colors # invert colors
    yarn # alternative node package manager
    zip # compress to zip
  ]
) ++ (
  with unstable; [
    ack # search in files
    autojump # jump into directory with j
    bat # cat replacement
    cargo # rust package manager
    ctop # top for containers
    dragon-drop # drag and drop
    dtrx # extract files
    entr # watch files
    exa # ls replacement
    fast-cli # speedtest
    fd # "find" for files
    fzf # fuzzy find tool
    ghcid # ghci daemon
    glow # markdown viewer
    hlint # haskell linter
    hyperfine # benchmark (time replacement)
    lazygit # git terminal ui
    lsd # ls replacement
    maim # screenshots
    mdcat # markdown viewer
    meld # git merge tool (graphical)
    mpv # video player
    nixpkgs-fmt # format nix
    pa_applet # volume tray icon
    pavucontrol # pulse audio settings
    ranger # file manager
    rclone # copy cloud to cloud
    ripgrep # fast grep
    screenkey # visualize typed keys
    shellcheck # linter for shell scripts
    slack-term # slack tui
    speedtest-cli # measure down/upload
    sxiv # image viewer
    translate-shell # trans command
    unclutter # hide idle mouse
    ytop # top/htop replacement
    zstd # compress tool used for .std
  ]
) ++ (
  with unstable.gitAndTools; [
    diff-so-fancy # git diff with colors
    git-gone # prune merged branches
    hub # github commandline tool
    tig # diff and comit view
  ]
) ++ (
  with unstable.elmPackages; [
    elm # compiler
    elm-format # elm pretty print
    elm-language-server # language-server
    elm-test # elm test runner
  ]
) ++ (
  with release2003.haskellPackages; [
    apply-refact # hlint: apply refactorings
    brittany # code formatter
    hoogle # function documentation
  ]
) ++ (
  with unstable.haskellPackages; [
    git-brunch # git checkout branch tui
    network-manager-tui # network tui
  ]
) ++ [ st ]
