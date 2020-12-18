with import <nixpkgs> { };

# Upstream has a `./vendor` directory with all deps which we rely upon.
buildGoPackage rec {
  name = "tty-share";
  version = "2.0.0";

  src = fetchFromGitHub {
    owner = "elisescu";
    repo = "tty-share";
    rev = "v${version}";
    sha256 = "1d2vd3d1lb4n0jq4s0p5mii1vz4r3z36hykr5mnx53srsni1wsj5";
  };

  goPackagePath = "github.com/elisescu/tty-share";

  meta = with stdenv.lib; {
    homepage = "https://tty-share.com";
    description = "Share your terminal with no effort and no tools on the remote side.";
    platforms = platforms.linux;
    license = licenses.mit;
  };
}
