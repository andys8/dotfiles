# Suckless terminal (andys8 fork)
with import <nixpkgs> {};
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
    rev = "82b38bf3c7669801d94b4fd32fb57bb1bd089468";
    sha256 = "1qm8lcyjsxx4w53yzsylz5bhszx3y00dqw12p4ah9cmg2nbk35vw";
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
