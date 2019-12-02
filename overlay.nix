self: super: {
  adventofcode = self.haskellPackages.callPackage ./derivation.nix{};
}
