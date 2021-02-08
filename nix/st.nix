# Suckless terminal (andys8 fork)
with import <nixpkgs> { };
with pkgs.lib;
let
  name = "st";
  version = "0.8.4-andys8";
in
stdenv.mkDerivation rec {
  inherit name;

  src = fetchFromGitHub {
    owner = "andys8";
    repo = "st";
    rev = "2de71357b862ae657a6f38d600c7d3d4fc2e82f7";
    sha256 = "1f2pg0x8jajvv6xmqkld7m07n7zj2rs0shk7nldgsdkql8g05apf";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ ncurses pkgs.xorg.libX11 pkgs.xorg.libXft.dev pkgs.xorg.libXcursor fontconfig harfbuzz ];

  installPhase = ''
    TERMINFO=$out/share/terminfo make install PREFIX=$out
    mkdir -p "$out"/share/applications
    cp "st.desktop" "$out"/share/applications/
  '';

  meta = with pkgs.lib; {
    homepage = https://github.com/andys8/st;
    description = "suckless terminal (andys8 fork)";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = maintainers;
  };
}
