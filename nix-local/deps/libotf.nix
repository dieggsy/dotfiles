{ stdenv, fetchurl, freetype }:


stdenv.mkDerivation rec {
  name = "libotf-0.9.13";

  buildInputs = [ freetype ];
  src = fetchurl {
    url = "http://download.savannah.gnu.org/releases/m17n/libotf-0.9.13.tar.gz";
    sha256 = "0239zvfan56w7vrppriwy77fzb10ag9llaz15nsraps2a2x6di3v";
  };

  doCheck = true;

  }
