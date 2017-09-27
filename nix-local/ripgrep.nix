{ stdenv, fetchFromGitHub, rustPlatform, makeWrapper }:

with rustPlatform;

buildRustPackage rec {
  name = "ripgrep-${version}";
  version = "0.6.0";

  src = fetchFromGitHub {
    owner = "BurntSushi";
    repo = "ripgrep";
    rev = "214f2bef666efd686b1be250fc8e9f54a2cebb0f";
    sha256 = "1m2pq5421y4l68fsxl65ys7aalvbyclw5x0n219ahpr840292icv";
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
