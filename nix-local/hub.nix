{ stdenv, fetchgit, go, Security, bundler, ruby, groff }:

stdenv.mkDerivation rec {
  name = "hub-${version}";
  version = "2.3.0-pre10";

  src = fetchgit {
    url = https://github.com/github/hub.git;
    rev = "refs/tags/v${version}";
    sha256 = "07sz1i6zxx2g36ayhjp1vjw523ckk5b0cr8b80s1qhar2d2hkibd";
  };

  buildInputs = [ go bundler ruby groff ] ++ stdenv.lib.optional stdenv.isDarwin Security;

  preInstall = ''
    patchShebangs .
  '';

  postInstall = ''
    mkdir -p "$out/share/zsh/site-functions"
    cp "etc/hub.zsh_completion" "$out/share/zsh/site-functions/_hub"

    mkdir -p "$out/etc/bash_completion.d"
    cp "etc/hub.bash_completion.sh" "$out/etc/bash_completion.d/"
  '';

  meta = with stdenv.lib; {
    description = "Command-line wrapper for git that makes you better at GitHub";

    license = licenses.mit;
    homepage = https://hub.github.com/;
    maintainers = with maintainers; [ the-kenny ];
    platforms = with platforms; unix;
  };
}
