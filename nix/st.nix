# Suckless terminal (andys8 fork)
with import <nixpkgs> { };
with pkgs.lib;
let
  name = "st";
  version = "0.8.5-andys8";
in
stdenv.mkDerivation rec {
  inherit name;

  src = fetchFromGitHub {
    owner = "andys8";
    repo = "st";
    rev = "e192e51d553a2c892de04eb970e0a66ccf582b69";
    sha256 = "0rn6yb4nacxy2dpg7mmf75pwc0hqzjlv9iv4ch1z13bfnpq7ypmk";
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
    maintainers = with maintainers; [ andys8 ];
  };
}
