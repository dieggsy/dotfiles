{ stdenv, fetchFromGitHub, otfcc, nodejs-8_x, ttfautohint, gnumake }:

stdenv.mkDerivation rec {
  name = "iosevka-${version}";
  version = "master";

  src = fetchFromGitHub {
    "owner" = "be5invis";
    "repo" = "iosevka";
    "rev" = "495620c57cdcc6f38a10658a0207d04134643cc3";
    "sha256" = "0mmdlrd9a0rhmmdqwkk6v7cdvbi23djr5kkiyv38llk11j3w0clp";
  };

  buildInputs = [ otfcc nodejs-8_x gnumake ttfautohint ];

  buildPhase = ''
    HOME=. npm install
    make custom-config set=term design='term v-asterisk-low' italic='v-i-serifed v-l-serifed v-a-doublestorey v-g-doublestorey'
    # make custom-config set=term design='term v-asterisk-low v-l-zshaped v-i-zshaped v-a-singlestorey v-g-singlestorey v-m-shortleg' italic='v-l-zshaped v-i-zshaped v-a-singlestorey v-g-singlestorey v-m-shortleg'
    # make custom-config set=term design='term v-asterisk-low v-l-zshaped v-i-zshaped v-a-doublestorey v-g-doublestorey v-m-shortleg' italic='v-l-zshaped v-i-zshaped v-a-doublestorey v-g-doublestorey v-m-shortleg'
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
