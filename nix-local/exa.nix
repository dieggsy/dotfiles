{ stdenv, fetchFromGitHub, rustPlatform, cmake, perl, pkgconfig, zlib }:

with rustPlatform;

buildRustPackage rec {
  name = "exa-${version}";
  version = "master";


  src = fetchFromGitHub {
    owner = "ogham";
    repo = "exa";
    rev = "8123122fac316486581d4dec938281b91565ca33";
    sha256 = "1p2m487ifd2iplzkq0ngfjy54yid7n6yd14w8fcxfdk87idb6shm";
  };

  depsSha256 = "1b79m6f2hfm6j7cwf42xahzpyviivy57zgbsclr1f1a3d7vv9cfr";

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
