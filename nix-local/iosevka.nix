{ stdenv, fetchFromGitHub, otfcc, nodejs-8_x, ttfautohint, gnumake }:

stdenv.mkDerivation rec {
  name = "iosevka-${version}";
  version = "master";

  src = fetchFromGitHub {
    "owner" = "be5invis";
    "repo" = "iosevka";
    "rev" = "96f3af9d8b78524f14c355eae6ee54896531b483";
    "sha256" = "1ry0473h7ld41izrcdkwk2vlvrrhqrqc4prca7x43wlqwr3x9rfw";
  };

  buildInputs = [ otfcc nodejs-8_x gnumake ttfautohint ];

  buildPhase = ''
    HOME=. npm install
    # make custom-config set=term design='term v-asterisk-low' italic='v-i-serifed v-l-serifed v-a-doublestorey v-g-doublestorey'
    make custom-config set=term design='term v-asterisk-low v-l-zshaped v-i-zshaped v-a-singlestorey v-g-singlestorey v-m-shortleg' italic='v-l-zshaped v-i-zshaped v-a-singlestorey v-g-singlestorey v-m-shortleg'
    make custom set=term
  '';

  installPhase = ''
    fontdir=$out/share/fonts/iosevka
    mkdir -p $fontdir

    cp -v dist/iosevka-term/ttf/* $fontdir
  '';

  postInstall = ''
    fc-cache -f -v
  '';

  meta = with stdenv.lib; {
    homepage = http://be5invis.github.io/Iosevka/;
    downloadPage = "https://github.com/be5invis/Iosevka/releases";
    description = ''
      Slender monospace sans-serif and slab-serif typeface inspired by Pragmata
      Pro, M+ and PF DIN Mono, designed to be the ideal font for programming.
    '';
    license = licenses.ofl;
    platforms = platforms.all;
  };
}
