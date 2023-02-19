# Suckless terminal (andys8 fork)
with import <nixpkgs> { };
with pkgs.lib;
let
  name = "st";
  version = "0.9-andys8";
in
stdenv.mkDerivation rec {
  inherit name;

  src = fetchFromGitHub {
    owner = "andys8";
    repo = "st";
    rev = "e52ca910d60acc0693815b704a330ae92f7b5e6a";
    sha256 = "sha256-RWRnlSb9/Sc4hO8+TZ+ZnSrTVMtKxWEY/s1xG9H4oKw=";
  };

  nativeBuildInputs = [ pkg-config ];
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
