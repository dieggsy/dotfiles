{ stdenv, go, fetchgit }:

let
  go-flags = fetchgit {
    url = "git://github.com/jessevdk/go-flags";
    rev = "f88afde2fa19a30cf50ba4b05b3d13bc6bae3079";
    sha256 = "15mvh5ah5483wdbc9s3iw9xh2svc9l3wnjaacr38ksj5cyzxhcx5";
  };
  go-runewidth = fetchgit {
    url = "git://github.com/mattn/go-runewidth";
    rev = "97311d9f7767e3d6f422ea06661bc2c7a19e8a5d";
    sha256 = "0dxlrzn570xl7gb11hjy1v4p3gw3r41yvqhrffgw95ha3q9p50cg";
  };
  termbox-go = fetchgit {
    url = "git://github.com/nsf/termbox-go";
    rev = "10cefba34bc5e7a6e5df0836a42513c28a10e074";
    sha256 = "05jy6dpva2a1xfsv3yajavbx41gx8hhh5k3k901dnk0118hnyd0w";
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
  "rev"= "2b3a18b5f0fb6b4f9190549597d3f962c02bc5eb";
  "sha256"= "07fd392kqyaj7fnl4sgzy7fcs0sw4jx3mx2khhgk64n9j9i37l59";
  "fetchSubmodules"= true;
};

in stdenv.mkDerivation rec {
  name = "peco-${version}";
  version = "master";

  src = fetchgit {
    url = "git://github.com/peco/peco";
    rev = "1403c233d2ffa8c29154db0d38eefa1cff0fe119";
    sha256 = "0jlp2dkjfqfq7ihxqnbskamg7z8bm2pbnq30pxxn99vdzsv27mjq";
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
