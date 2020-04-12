# Suckless terminal (andys8 fork)
with import <nixpkgs> {};
with stdenv.lib;
let
  name = "st";
  version = "0.8.2-andys8";
in
stdenv.mkDerivation rec {
  inherit name;

  src = fetchFromGitHub {
    owner = "andys8";
    repo = "st";
    rev = "3f7163941693b46d259a4a27996c90bffa7aa52e";
    sha256 = "14d6030n9n0f7l7ha66h3g3a9m7s4vncbk5b34hbzi2bhzn5hicq";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ ncurses pkgs.xorg.libX11 pkgs.xorg.libXft.dev pkgs.xorg.libXcursor fontconfig ];

  installPhase = ''
    TERMINFO=$out/share/terminfo make install PREFIX=$out
    mkdir -p "$out"/share/applications
    cp "st.desktop" "$out"/share/applications/
  '';

  meta = with stdenv.lib; {
    homepage = https://github.com/andys8/st;
    description = "suckless terminal (andys8 fork)";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = maintainers;
  };
}
