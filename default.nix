{ mkDerivation, base, classy-prelude, hspec, parsec, QuickCheck
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "semver-range";
  version = "0.2.7";
  src = ./.;
  libraryHaskellDepends = [
    base classy-prelude parsec text unordered-containers
  ];
  testHaskellDepends = [
    base classy-prelude hspec parsec QuickCheck text
    unordered-containers
  ];
  description = "An implementation of semver and semantic version ranges";
  license = stdenv.lib.licenses.mit;
}
