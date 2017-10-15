{ stdenv, fetchFromGitHub, rustPlatform, makeWrapper }:

with rustPlatform;

buildRustPackage rec {
  name = "ripgrep-${version}";
  version = "master";

  src = fetchFromGitHub {
    owner = "BurntSushi";
    repo = "ripgrep";
    rev = "a98156e71cc2aefd2d3e18c76c6d09cc1ef8c5ac";
    sha256 = "16nmbrl07p0cpmv5lfi8b8kss2c3ynnrjrjqz0mp5qp935r1lh62";
  };

  depsSha256 = "1g2qm9mdnh79iqwldywa0h81nvxvv3wdpvxns60f4nw9iqz6a7pm";

  preFixup = ''
    mkdir -p "$out/man/man1"
    cp "$src/doc/rg.1" "$out/man/man1"
  '';

  meta = with stdenv.lib; {
    description = "A utility that combines the usability of The Silver Searcher with the raw speed of grep";
    homepage = https://github.com/BurntSushi/ripgrep;
    license = with licenses; [ unlicense ];
    maintainers = [ maintainers.tailhook ];
    platforms = platforms.all;
  };
}
