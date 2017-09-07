{ stdenv, fetchFromGitHub, rustPlatform, makeWrapper }:

with rustPlatform;

buildRustPackage rec {
  name = "ripgrep-${version}";
  version = "0.6.0";

  src = fetchFromGitHub {
    owner = "BurntSushi";
    repo = "ripgrep";
    rev = "beb010d004a79eedb8baa6f4eab21532e83a0ef0";
    sha256 = "0v23zj4yxky977xvsx9zjc5n0pkclnqjdf58kfniap49v1d22gcs";
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
