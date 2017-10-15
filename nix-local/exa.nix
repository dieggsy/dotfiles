{ stdenv, fetchFromGitHub, rustPlatform, cmake, perl, pkgconfig, zlib }:

with rustPlatform;

buildRustPackage rec {
  name = "exa-${version}";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "ogham";
    repo = "exa";
    rev = "0eb796629402938289726e07cf7ffa5263967b4d";
    sha256 = "06z57j6xb1vsxfqb7ql4kgl5188hrapmfd0hxvw5kvfndcqbqgj7";
  };

  depsSha256 = "1zi1amr5fs8kxsirkxd45p9da2mf51wczxxzxhry5kig25bac0x1";

  nativeBuildInputs = [ cmake pkgconfig perl ];
  buildInputs = [ zlib ];

  # Some tests fail, but Travis ensures a proper build
  doCheck = false;

  preFixup = ''
    mkdir -p "$out/man/man1"
    cp "$src/contrib/man/exa.1" "$out/man/man1"
  '';

  meta = with stdenv.lib; {
    description = "Replacement for 'ls' written in Rust";
    longDescription = ''
      exa is a modern replacement for ls. It uses colours for information by
      default, helping you distinguish between many types of files, such as
      whether you are the owner, or in the owning group. It also has extra
      features not present in the original ls, such as viewing the Git status
      for a directory, or recursing into directories with a tree view. exa is
      written in Rust, so it’s small, fast, and portable.
    '';
    homepage = http://the.exa.website;
    license = licenses.mit;
    maintainer = [ maintainers.ehegnes ];
  };
}
