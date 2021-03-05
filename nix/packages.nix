let
  unstable = import <nixpkgs> { };
  release2003 = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") { };
  release2009 = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz") { };
in
(
  with release2009; [
    any-nix-shell # use fish as nix-shell
    arandr # gui for monitors with xrandr
    asciinema # record terminal
    asciiquarium # terminal aquarium animation
    autorandr # xrandr monitor profiles
    awscli2 # amazon web services
    bc # calculator
    cabal2nix # Convert cabal files to nix
    chafa # preview images in terminal
    coursier # scala build tool
    diskus # disk usage
    dtrx # extract files
    elmPackages.elm # compiler
    elmPackages.elm-format # elm pretty print
    elmPackages.elm-language-server # language-server
    elmPackages.elm-live # dev server
    elmPackages.elm-test # elm test runner
    fast-cli # speedtest
    ffmpeg # convert videos
    gcolor3 # color picker
    gifsicle # used by asciicast2gif
    gnome3.zenity # show ui messages and dialogs
    haskellPackages.hoogle # function documentation
    hasmail # mail imap notifier for new mails
    iotop # io usage
    jq # json processor
    lsof # list open files/ports
    lxqt.pavucontrol-qt # control volume per application
    mdcat # markdown viewer
    mdp # markdown presentation
    ncdu # disk usage
    neofetch # system information
    nethogs # network traffic monitoring
    nitrogen # Wallpaper
    nix # nix package manager
    pass-otp # pass otp for token
    rofi # dmenu replacement
    sbt # sbt package manager
    scala # scala compiler
    screenfetch # system information
    sent # suckless presentation tool
    shfmt # format shell scripts
    simplescreenrecorder # screen recorder
    time # measure time
    tmatrix # terminal matrix animation
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
  with release2003; [
    lsd # ls replacement
  ]
) ++ (
  with unstable; [
    ack # search in files
    autojump # jump into directory with j
    bat # cat replacement
    bottom # system monitor
    cargo # rust package manager
    ctop # top for containers
    dhall # dhall (format)
    dragon-drop # drag and drop
    duf # disk usage
    dunst # notification daemon
    entr # watch files
    exa # ls replacement
    fd # "find" for files
    fzf # fuzzy find tool
    ghcid # ghci daemon
    gitAndTools.delta # git diff viewer
    gitAndTools.diff-so-fancy # git diff with colors
    gitAndTools.gh # github-cli
    gitAndTools.git-gone # prune merged branches
    gitAndTools.hub # github commandline tool
    gitAndTools.tig # diff and comit view
    glow # markdown viewer
    haskellPackages.brittany # code formatter
    haskellPackages.git-brunch # git checkout branch tui
    haskellPackages.network-manager-tui # network tui
    hlint # haskell linter
    httpie # curl alternative
    hyperfine # benchmark (time replacement)
    lazygit # git terminal ui
    maim # screenshots
    meld # git merge tool (graphical)
    mpv # video player
    nixpkgs-fmt # format nix
    nodejs-15_x # nodejs and npm
    pa_applet # volume tray icon
    pavucontrol # pulse audio settings
    python39Packages.cfn-lint # cloudformation linter
    ranger # file manager
    rclone # copy cloud to cloud
    ripgrep # fast grep
    screenkey # visualize typed keys
    shellcheck # linter for shell scripts
    slack-term # slack tui
    speedtest-cli # measure down/upload
    sxiv # image viewer
    translate-shell # trans command
    tty-share # share terminal
    ueberzug # draw images in terminal
    unclutter # hide idle mouse
    xmobar # status bar for xmonad
    ytop # top/htop replacement
    zstd # compress tool used for .std
  ]
) ++ [
  (import ./st.nix)
]
