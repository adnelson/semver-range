{ mkDerivation, base, classy-prelude, parsec, stdenv, text
, cabal-install, unordered-containers, QuickCheck, hspec }:
mkDerivation {
  pname = "semver-range";
  version = "0.2.6";
  src = ./.;
  isLibrary = true;
  buildDepends = [ base classy-prelude parsec text cabal-install
                   unordered-containers QuickCheck hspec ];
  description = "An implementation of semver and semantic version ranges";
  license = stdenv.lib.licenses.mit;
}
