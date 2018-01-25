{ stdenv, fetchFromGitHub, python3, setuptools, python3Packages }:

stdenv.mkDerivation rec {
  name = "pass-git-helper-${version}";
  version = "master";

  src = fetchFromGitHub {
    owner = "languitar";
    repo = "pass-git-helper";
    rev = "0d7712f4bb1ade0dfec1816aff40334929771c08";
    sha256  = "1nw8ziy6f5ahj41ibcnp6z4aq23f43p3bij2fp5zk3gggcd5mzvh";
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
