{ mkDerivation, base, classy-prelude, parsec, stdenv, text
, cabal-install, unordered-containers }:
mkDerivation {
  pname = "semver-range";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  buildDepends = [ base classy-prelude parsec text cabal-install
                   unordered-containers ];
  description = "An implementation of semver and semantic version ranges";
  license = stdenv.lib.licenses.mit;
}
