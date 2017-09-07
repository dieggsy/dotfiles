{ stdenv, fetchFromGitHub, python3, setuptools, python3Packages }:

stdenv.mkDerivation rec {
  name = "pass-git-helper-${version}";
  version = "0.5-dev";

  src = fetchFromGitHub {
    owner = "languitar";
    repo = "pass-git-helper";
    rev = "5fadeec780c0c3e5ba2442dfa0933f3bdb7cf421";
    sha256  = "0fxf0b66qqd1kksyzrrlfzs7960ryaq9lr5an6vp2qxpsax3y5fz";
  };

  buildInputs = [ python3 setuptools python3Packages.wrapPython ];

  installPhase = ''
    mkdir -p $out/lib/${python3.libPrefix}/site-packages
    export PYTHONPATH="$out/lib/${python3.libPrefix}/site-packages:$PYTHONPATH"
    python3 setup.py install --prefix $out
  '';

  postFixup = ''
    echo $(toPythonPath $out)
    makeWrapperArgs="\
      --prefix PYTHONPATH : \"$(toPythonPath $out):$(toPythonPath ${setuptools})\""
    wrapPythonPrograms
  '';

  meta = with stdenv.lib; {
    description = "A git credential helper interfacing with pass, the standard unix password manager.";
    license = licenses.lgpl3;
  };
}
