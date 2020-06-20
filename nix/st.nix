# Suckless terminal (andys8 fork)
with import <nixpkgs> { };
with stdenv.lib;
let
  name = "st";
  version = "0.8.3-andys8";
in
stdenv.mkDerivation rec {
  inherit name;

  src = fetchFromGitHub {
    owner = "andys8";
    repo = "st";
    rev = "73b8746d585bd2cb706156c4ae79c9b056cad837";
    sha256 = "1xp628j0y6ip7ng89lgrd6c1x7lvd4v6dd33gd5nr5gz7kgasndi";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ ncurses pkgs.xorg.libX11 pkgs.xorg.libXft.dev pkgs.xorg.libXcursor fontconfig harfbuzz ];

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
