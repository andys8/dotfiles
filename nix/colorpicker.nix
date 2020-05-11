# Colorpicker
with import <nixpkgs> {};
with stdenv.lib;
let
  name = "colorpicker";
in
stdenv.mkDerivation rec {
  inherit name;

  src = fetchFromGitHub {
    owner = "Jack12816";
    repo = "colorpicker";
    rev = "a4455b92fde1dfbac81e7852f171093932154a30";
    sha256 = "13bb7zz45ad46sqs74khc8bwx1lk6j49i3y7b4n6gw8g6b2sqrng";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ pkgs.xorg.libX11 pkgs.gtk2 pkgs.libcanberra-gtk2 ];

  installPhase = ''
    make colorpicker
    mkdir -p "$out"/bin/
    cp colorpicker "$out"/bin/
  '';

  meta = with stdenv.lib; {
    homepage = https://github.com/Jack12816/colorpicker;
    description = "Click on a pixel on your screen and print its color value in RGB";
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = maintainers;
  };
}
