{ mkDerivation, base, cmdargs, containers, mtl, sbv, stdenv, vector
}:
mkDerivation {
  pname = "adventOfCode2019";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cmdargs containers mtl sbv vector
  ];
  homepage = "https://adventofcode.com/2019/";
  description = "Solutions to Advent of Code 2019";
  license = stdenv.lib.licenses.mit;
}
