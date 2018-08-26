{ mkDerivation, base, containers, filepath, hakyll, old-locale
, pandoc, stdenv, time
}:
mkDerivation {
  pname = "LambdaLog";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers filepath hakyll old-locale pandoc time
  ];
  homepage = "http://lambdalog.seanseefried.com";
  description = "My blog";
  license = stdenv.lib.licenses.bsd3;
}
