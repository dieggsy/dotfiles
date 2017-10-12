{ stdenv, fetchFromGitHub, rustPlatform, makeWrapper }:

with rustPlatform;

buildRustPackage rec {
  name = "ripgrep-${version}";
  version = "master";

  src = fetchFromGitHub {
    owner = "BurntSushi";
    repo = "ripgrep";
    rev = "e7c06b92fb996adcbc40d1f025e29c44de5383e8";
    sha256 = "1gshqx71p6mdbhs37wrp35rd3rlis7mmmvmamrhmxiamki2s2hc0";
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
