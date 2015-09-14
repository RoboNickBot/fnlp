{ mkDerivation, base, charset, containers, parsec, stdenv, text }:
mkDerivation {
  pname = "funlp";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base charset containers parsec text ];
  description = "purely [Fun]ctional natural [L]anguage [P]rocessing";
  license = stdenv.lib.licenses.gpl3;
}
