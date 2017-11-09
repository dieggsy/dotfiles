{ stdenv, fetchgit, pkgconfig, vte, gtk3, ncurses, makeWrapper, symlinkJoin
, configFile ? null
}:

let
  version = "master";
  termite = stdenv.mkDerivation {
  name = "termite-${version}";

  src = fetchgit {
    url = "https://github.com/thestinger/termite";
    rev = "1f9ab29f74e6e4aded1941d53a19495f5460ad87";
    sha256 = "1lw6mxgj67zrmmqih6ms5y0a2rk327smgsz20hcgg18sxss2gya5";
    fetchSubmodules = true;
  };

  postPatch = "sed '1i#include <math.h>' -i termite.cc";

  makeFlags = [ "VERSION=v${version}" "PREFIX=" "DESTDIR=$(out)" ];

  buildInputs = [ pkgconfig vte gtk3 ncurses ];

  outputs = [ "out" "terminfo" ];

  postInstall = ''
    mkdir -p $terminfo/share
    mv $out/share/terminfo $terminfo/share/terminfo

    mkdir -p $out/nix-support
    echo "$terminfo" >> $out/nix-support/propagated-user-env-packages
  '';

  meta = with stdenv.lib; {
    description = "A simple VTE-based terminal";
    license = licenses.lgpl2Plus;
    homepage = https://github.com/thestinger/termite/;
    maintainers = with maintainers; [ koral garbas ];
    platforms = platforms.all;
  };
};
in if configFile == null then termite else symlinkJoin {
  name = "termite-with-config-${version}";
  paths = [ termite ];
  nativeBuildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/termite \
      --add-flags "--config ${configFile}"
  '';
  passthru.terminfo = termite.terminfo;
}
