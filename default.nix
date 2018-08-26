{ mkDerivation, aeson, base, containers, filepath, hakyll, pandoc
, stdenv, text, time, unordered-containers
}:
mkDerivation {
  pname = "LambdaLog";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers filepath hakyll pandoc text time
    unordered-containers
  ];
  homepage = "http://lambdalog.seanseefried.com";
  description = "My blog";
  license = stdenv.lib.licenses.bsd3;
}
