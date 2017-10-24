{ stdenv, fetchFromGitHub, rustPlatform, makeWrapper }:

with rustPlatform;

buildRustPackage rec {
  name = "ripgrep-${version}";
  version = "master";

  src = fetchFromGitHub {
    owner = "BurntSushi";
    repo = "ripgrep";
    rev = "1aec4b11231ccb7de92e3008408e4d06a714d106";
    sha256 = "0s10b5c0kbi0x7avymia93k5w48jmha43d68wjgn94wcl2qpgbaf";
  };

  depsSha256 = "0jaz4zbq40kx8srfg9qzgcfgpia7d51kh0n0hiax0740p9yrhyrz";

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
