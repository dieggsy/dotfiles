{ stdenv, fetchurl, unzip, premake5 }:

stdenv.mkDerivation rec {
  name = "otfcc-${version}";
  version = "0.9.2";

  src = fetchurl {
    url = "https://github.com/caryll/otfcc/archive/v${version}.tar.gz";
    sha256= "1fm7xn1yi677jbpmxrrcbl52qppk2z5cwngysznbp6hqkic0dqin";
  };

  nativeBuildInputs = [ unzip premake5 ];

  buildPhase = ''
    premake5 gmake
    (cd build/gmake && make config=release_x64)
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/release-x64/otfccdump $out/bin/otfccdump
    cp bin/release-x64/otfccbuild $out/bin/otfccbuild
  '';

  meta = with stdenv.lib; {
    homepage = "https://github.com/caryll/otfcc";
    description = "A library and utility used for parsing and writing OpenType font files";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
}
