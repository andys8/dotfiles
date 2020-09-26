with import <nixpkgs> { };
rustPlatform.buildRustPackage rec {
  name = "bottom";
  version = "0.4.7";

  src = fetchgit {
    url = "https://github.com/ClementTsang/bottom";
    rev = "${version}";
    sha256 = "178z9f2z861rni8zqrp4w45jmr8g325jfgwz5765sbvvf7jhjdxc";
  };

  cargoSha256 = "0gwmwzx0s9nv96d4sdkwd84chgff1qs782hdywl0gmavgaxnmfws";

  meta = with stdenv.lib; {
    homepage = "https://github.com/ClementTsang/bottom";
    description = "Yet another cross-platform graphical process/system monitor";
    platforms = platforms.linux;
    license = licenses.mit;
  };
}
