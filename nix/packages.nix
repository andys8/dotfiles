let
  unstable = import <nixpkgs> { };
  release2311 = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/23.11.tar.gz") { };
in
(
  with release2311; [
    (rofi.override { plugins = [ rofi-emoji ]; }) # rofi with emoji plugin
    any-nix-shell # use fish as nix-shell
    arandr # gui for monitors with xrandr
    asciinema # record terminal
    asciiquarium # terminal aquarium animation
    autorandr # xrandr monitor profiles
    awscli2 # amazon web services
    baobab # disk usage graphical
    bc # calculator
    cabal2nix # convert cabal files to nix
    chafa # preview images in terminal
    colorized-logs # colorize logs
    diskus # disk usage
    dtrx # extract files
    elmPackages.elm # compiler
    elmPackages.elm-format # elm pretty print
    elmPackages.elm-language-server # language-server
    elmPackages.elm-test # elm test runner
    fast-cli # speedtest
    ffmpeg # convert videos
    gcolor3 # color picker
    gifsicle # used by asciicast2gif
    gitAndTools.tig # diff and comit view
    gnome3.zenity # show ui messages and dialogs
    guvcview # show webcam on screen
    haskellPackages.cabal-fmt # cabal formatter
    haskellPackages.fourmolu # haskell formatter
    haskellPackages.hoogle # function documentation
    hasmail # mail imap notifier for new mails
    htop-vim # htop with vim keys
    httpie # curl alternative
    iotop # io usage
    jq # json processor
    jwt-cli # encode/decode jwts
    keepassxc # password manager
    keynav # keyboard mouse navigation
    kondo # clean project caches
    languagetool # language tool like grammarly
    libreoffice # office
    lsd # ls replacement
    lsof # list open files/ports
    lxqt.pavucontrol-qt # control volume per application
    mdcat # markdown viewer
    mdp # markdown presentation
    mongodb-tools # mongo cli tools
    ncdu # disk usage
    neofetch # system information
    nethogs # network traffic monitoring
    nitrogen # wallpaper
    nix # nix package manager
    ouch # compress tool
    python39Packages.cfn-lint # cloudformation linter
    robo3t # mongodb gui
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
    udiskie # usb device automount
    unzip # extract zip
    vokoscreen # screen recorder
    watch # repeat command
    wcalc # calculator
    xdo # x tool used in scripts
    xdragon # drag and drop
    xmonad-with-packages # window manager in haskell
    xrandr-invert-colors # invert colors
    zip # compress to zip
  ]
) ++ (
  with unstable; [
    ack # search in files
    act # run github actions locally
    bat # cat replacement
    bottom # system monitor
    cargo # rust package manager
    cargo-cache # cache clean
    ctop # top for containers
    dhall # dhall (format)
    duf # disk usage
    dunst # notification daemon
    entr # watch files
    exa # ls replacement
    fd # find for files
    fzf # fuzzy find tool
    ghcid # ghci daemon
    gitAndTools.delta # git diff viewer
    gitAndTools.diff-so-fancy # git diff with colors
    gitAndTools.gh # github-cli
    gitAndTools.git-gone # prune merged branches
    gitAndTools.hub # github commandline tool
    glow # markdown viewer
    go # go language
    haskellPackages.git-brunch # git checkout branch tui
    hlint # haskell linter
    hyperfine # benchmark (time replacement)
    lazygit # git terminal ui
    maim # screenshots
    meld # git merge tool (graphical)
    nb # notes and todo
    nixpkgs-fmt # format nix
    pa_applet # volume tray icon
    pavucontrol # pulse audio settings
    ranger # file manager
    rclone # copy cloud to cloud
    ripgrep # fast grep
    rustfmt # rust formatter
    screenkey # visualize typed keys
    shell-gpt # TheR1D/shell_gpt (sgpt)
    shellcheck # linter for shell scripts
    speedtest-cli # measure down/upload
    sxiv # image viewer
    translate-shell # trans command
    tty-share # share terminal
    ueberzugpp # replacement for ueberzug (images in terminal)
    unclutter # hide idle mouse
    xmobar # status bar for xmonad
    zoxide # jump into directory with z
  ]
) ++ [
  (import ./st.nix)
]
