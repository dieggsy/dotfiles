{ stdenv, fetchgit, go, Security }:

stdenv.mkDerivation rec {
  name = "hub-${version}";
  version = "2.3.0-pre10";

  src = fetchgit {
    url = https://github.com/github/hub.git;
    rev = "refs/tags/v${version}";
    sha256 = "07sz1i6zxx2g36ayhjp1vjw523ckk5b0cr8b80s1qhar2d2hkibd";
  };


  buildInputs = [ go ] ++ stdenv.lib.optional stdenv.isDarwin Security;

  phases = [ "unpackPhase" "buildPhase" "installPhase" ];

  buildPhase = ''
    patchShebangs .
    sh script/build
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    cp bin/hub "$out/bin/"

    mkdir -p "$out/share/man/man1"
    cp "man/hub.1" "$out/share/man/man1/"

    mkdir -p "$out/share/zsh/site-functions"
    cp "etc/hub.zsh_completion" "$out/share/zsh/site-functions/_hub"

    mkdir -p "$out/etc/bash_completion.d"
    cp "etc/hub.bash_completion.sh" "$out/etc/bash_completion.d/"

# Should we also install provided git-hooks?
# ?
  '';

  meta = with stdenv.lib; {
    description = "Command-line wrapper for git that makes you better at GitHub";

    license = licenses.mit;
    homepage = https://hub.github.com/;
    maintainers = with maintainers; [ the-kenny ];
    platforms = with platforms; unix;
  };
}
