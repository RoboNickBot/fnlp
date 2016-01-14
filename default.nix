{ mkDerivation, base, charset, containers, convertible, directory
, filepath, HDBC, HDBC-sqlite3, parsec, pipes, stdenv, strict, text
}:
mkDerivation {
  pname = "fnlp";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base charset containers convertible directory filepath HDBC
    HDBC-sqlite3 parsec pipes strict text
  ];
  description = "purely functional natural language processing";
  license = stdenv.lib.licenses.gpl3;
}
