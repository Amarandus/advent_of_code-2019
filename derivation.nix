{ mkDerivation, base, cmdargs, containers, stdenv }:
mkDerivation {
  pname = "adventOfCode2019";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cmdargs containers ];
  homepage = "https://adventofcode.com/2019/";
  description = "Solutions to Advent of Code 2019";
  license = stdenv.lib.licenses.mit;
}
