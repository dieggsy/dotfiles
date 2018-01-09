{ stdenv, fetchFromGitHub, python3, setuptools, python3Packages }:

stdenv.mkDerivation rec {
  name = "pass-git-helper-${version}";
  version = "master";

  src = fetchFromGitHub {
    owner = "languitar";
    repo = "pass-git-helper";
    rev = "f8f72671243a29af3998e0087411dc9a0d3d32e1";
    sha256  = "1nqpsmwjnjzhvkq5jw3pnsj95amj96rmvryw8gzgih51cbd86jxh";
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
