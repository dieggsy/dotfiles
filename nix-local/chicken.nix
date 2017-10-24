{ stdenv, fetchurl, makeWrapper }:

let
  version = "4.12.0";
  platform = with stdenv;
    if isDarwin then "macosx"
    else if isCygwin then "cygwin"
    else if (isFreeBSD || isOpenBSD) then "bsd"
    else if isSunOS then "solaris"
    else "linux"; # Should be a sane default
  lib = stdenv.lib;
in
stdenv.mkDerivation {
  name = "chicken-${version}";

  binaryVersion = 8;

  src = fetchurl {
    url = "http://code.call-cc.org/releases/${version}/chicken-${version}.tar.gz";
    sha256 = "12b9gaa9lqh39lj1v4wm48f6z8ww3jdkvc5bh9gqqvn6kd2wwnk0";
  };

  buildFlags = "PLATFORM=${platform} PREFIX=$(out) VARDIR=$(out)/lib";
  installFlags = "PLATFORM=${platform} PREFIX=$(out) VARDIR=$(out)/lib";

  buildInputs = [ makeWrapper ];

  postInstall = ''
    for f in $out/bin/*
    do
      wrapProgram $f \
        --prefix PATH : ${stdenv.cc}/bin
    done
  '';

  meta = {
    homepage = http://www.call-cc.org/;
    license = stdenv.lib.licenses.bsd3;
    maintainers = with stdenv.lib.maintainers; [ the-kenny ];
    platforms = with stdenv.lib.platforms; allBut darwin;
    description = "A portable compiler for the Scheme programming language";
    longDescription = ''
      CHICKEN is a compiler for the Scheme programming language.
      CHICKEN produces portable and efficient C, supports almost all
      of the R5RS Scheme language standard, and includes many
      enhancements and extensions. CHICKEN runs on Linux, macOS,
      Windows, and many Unix flavours.
    '';
  };
}
