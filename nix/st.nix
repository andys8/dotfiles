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
    rev = "c7a45fedd03b9689c9ba0ef0f11a186265df76e8";
    sha256 = "0xcscgckcn2kzrrdszdq6pljgvzr226lv2v64gw1dcs1yvp8mw6z";
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
