with import <nixpkgs> { };
buildGoPackage rec {
  name = "tty-share";
  version = "0.6.2";

  src = fetchgit {
    url = "https://github.com/elisescu/tty-share";
    rev = "v${version}";
    sha256 = "09f42zkxs1si60ljrs2lbnallhk9sncj3mv8j6d1qmg2mzfak3c5";
  };

  goPackagePath = "github.com/elisescu/tty-share";

  meta = with stdenv.lib; {
    homepage = "https://tty-share.com";
    description = "Share your terminal with no effort and no tools on the remote side.";
    platforms = platforms.linux;
    license = licenses.mit;
  };
}
