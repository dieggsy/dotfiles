{ stdenv, go, fetchgit }:

let
  go-flags = fetchgit {
    url = "git://github.com/jessevdk/go-flags";
    rev = "6cf8f02b4ae8ba723ddc64dcfd403e530c06d927";
    sha256 = "1wxyywnx4jynjkyagvcaypbiaq27w0sw3jzmcfgmrfkhi0763sh8";
  };
  go-runewidth = fetchgit {
    url = "git://github.com/mattn/go-runewidth";
    rev = "97311d9f7767e3d6f422ea06661bc2c7a19e8a5d";
    sha256 = "0dxlrzn570xl7gb11hjy1v4p3gw3r41yvqhrffgw95ha3q9p50cg";
  };
  termbox-go = fetchgit {
    url = "git://github.com/nsf/termbox-go";
    rev = "4ed959e0540971545eddb8c75514973d670cf739";
    sha256 = "1vx64i1mg660if3wwm81p4b7lzxfb3qbr39i7misdyld3fc486p9";
  };
google-btree = fetchgit {
  "url"= "git://github.com/google/btree";
  "rev"= "316fb6d3f031ae8f4d457c6c5186b9e3ded70435";
  "sha256"= "1fyj10cy2d37mpfk73hjfjwpsgpnmdzf2mrkkvzyx0d41sf46xfd";
  "fetchSubmodules" = true;
};
go-pdebug = fetchgit {
  "url" = "git://github.com/lestrrat/go-pdebug";
  "rev" = "2e6eaaa5717f81bda41d27070d3c966f40a1e75f";
  "sha256" = "05dx1f1yln47nazyi50zix82xbnyva3hily4gh5gaz021h01npax";
  "fetchSubmodules" = true;
};
errors = fetchgit {
  "url"= "git://github.com/pkg/errors";
  "rev"= "c605e284fe17294bda444b34710735b29d1a9d90";
  "sha256"= "1izjk4msnc6wn1mclg0ypa6i31zfwb1r3032k8q4jfbd57hp0bz6";
  "fetchSubmodules"= true;
};

in stdenv.mkDerivation rec {
  name = "peco-${version}";
  version = "master";

  src = fetchgit {
    url = "git://github.com/peco/peco";
    rev = "7b7749376ff732b10c2a2f70252dad41081ca23a";
    sha256 = "079lx0a6lwa94gczv0j9ppys33a3ig6vpah8gv4b8sg6md2ww13s";
  };

  buildInputs = [ go ];

  sourceRoot = ".";

  buildPhase = ''
    mkdir -p src/github.com/jessevdk/go-flags/
    ln -s ${go-flags}/* src/github.com/jessevdk/go-flags

    mkdir -p src/github.com/mattn/go-runewidth/
    ln -s ${go-runewidth}/* src/github.com/mattn/go-runewidth

    mkdir -p src/github.com/nsf/termbox-go/
    ln -s ${termbox-go}/* src/github.com/nsf/termbox-go

    mkdir -p src/github.com/google/btree
    ln -s ${google-btree}/* src/github.com/google/btree

    mkdir -p src/github.com/lestrrat/go-pdebug
    ln -s ${go-pdebug}/* src/github.com/lestrrat/go-pdebug

    mkdir -p src/github.com/pkg/errors
    ln -s ${errors}/* src/github.com/pkg/errors

    mkdir -p src/github.com/peco/peco
    ln -s ${src}/* src/github.com/peco/peco

    export GOPATH=$PWD
    go build -v -o peco src/github.com/peco/peco/cmd/peco/peco.go
  ''; # */

  installPhase = ''
    ensureDir $out/bin
    cp peco $out/bin
  '';

  meta = with stdenv.lib; {
    description = "Simplistic interactive filtering tool";
    homepage = https://github.com/peco/peco;
    license = licenses.mit;
    # peco should work on Windows or other POSIX platforms, but the go package
    # declares only linux and darwin.
    platforms = platforms.linux ++ platforms.darwin;
  };
}
