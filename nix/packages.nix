let
  unstable = import <nixpkgs> { };
  release2003 = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") { };
in
(
  with release2003; [
    arandr # gui for monitors with xrandr
    asciinema # record terminal
    autorandr # xrandr monitor profiles
    awscli # amazon web services
    cabal2nix # Convert cabal files to nix
    chafa # preview images in terminal
    chromium # web browser
    coursier # scala build tool
    diskus # disk usage
    dtrx # extract files
    dunst # notification daemon
    emacs # alternative editor
    fast-cli # speedtest
    ffmpeg # convert videos
    gcolor3 # color picker
    gnome3.zenity # show ui messages and dialogs
    hasmail # mail imap notifier for new mails
    i3status-rust # i3 status bar
    jq # json processor
    lsd # ls replacement
    lsof # list open files/ports
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
    simplescreenrecorder # screen recorder
    termonad-with-packages # haskell terminal
    time # measure time
    toilet # ascii art fonts
    tokei # count code lines
    trash-cli # rm into trash folder
    trayer # tray icons
    unzip # extract zip
    vokoscreen # screen recorder
    watch # repeat command
    wcalc # calculator
    xdo # x tool used in scripts
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
    dhall # dhall (format)
    dragon-drop # drag and drop
    entr # watch files
    exa # ls replacement
    fd # "find" for files
    fzf # fuzzy find tool
    ghcid # ghci daemon
    glow # markdown viewer
    hlint # haskell linter
    hyperfine # benchmark (time replacement)
    lazygit # git terminal ui
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
    ueberzug # draw images in terminal
    unclutter # hide idle mouse
    xmobar # status bar for xmonad
    ytop # top/htop replacement
    zstd # compress tool used for .std
  ]
) ++ (
  with unstable.gitAndTools; [
    diff-so-fancy # git diff with colors
    gh # github-cli
    git-gone # prune merged branches
    hub # github commandline tool
    tig # diff and comit view
  ]
) ++ (
  with unstable.elmPackages; [
    elm # compiler
    elm-format # elm pretty print
    elm-language-server # language-server
    elm-live # dev server
    elm-test # elm test runner
  ]
) ++ (
  with release2003.haskellPackages; [
    brittany # code formatter
    hoogle # function documentation
  ]
) ++ (
  with unstable.haskellPackages; [
    git-brunch # git checkout branch tui
    network-manager-tui # network tui
  ]
) ++ [
  (import ./st.nix)
]
