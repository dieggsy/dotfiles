{ stdenv, fetchFromGitHub, rustPlatform, cmake, perl, pkgconfig, zlib }:

with rustPlatform;

buildRustPackage rec {
  name = "exa-${version}";
  version = "0.8.0";


  src = fetchFromGitHub {
    owner = "ogham";
    repo = "exa";
    rev = "547ceda15b4acff640c3fa24705d0ceaf9321b2b";
    sha256 = "1piin1cxhml8w76hlmzbq3lxzbxg444pa1b7r30p84578hx27aly";
  };

  depsSha256 = "0yz41prkjs5rmvdhr9k58a52l7hvwy5mfg8rcpsq4ybgf601lja2";

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
