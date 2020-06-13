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
    rev = "bd5c8e7d31311b6d49f3e3a2d071f7a4be7ebc35";
    sha256 = "1ik5cw8jy6jifwl27m626nlvqxh5kixxph3cf4ibjnrg74k6z611";
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
