let
  fetchNixPkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixPkgs {
    rev = "1d4de0d552ae9aa66a5b8dee5fb0650a4372d148";
    sha256 = "09qx58dp1kbj7cpzp8ahbqfbbab1frb12sh1qng87rybcaz0dz01";
    outputSha256 = "0xpqc1fhkvvv5dv1zmas2j1q27mi7j7dgyjcdh82mlgl1q63i660";
  };

  pkgs = import nixpkgs { };

in
  pkgs.haskellPackages.callPackage ./default.nix { }