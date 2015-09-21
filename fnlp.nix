{ mkDerivation, base, charset, containers, convertible, directory
, filepath, HDBC, HDBC-sqlite3, parsec, pipes, stdenv, strict, text
, criterion
}:
mkDerivation {
  pname = "fnlp";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base charset containers convertible directory filepath HDBC
    HDBC-sqlite3 parsec pipes strict text criterion
  ];
  testHaskellDepends = [ base ];
  description = "purely functional natural language processing";
  license = stdenv.lib.licenses.gpl3;
}
