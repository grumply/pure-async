{ mkDerivation, base, pure-elm, stdenv }:
mkDerivation {
  pname = "pure-async";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-elm ];
  homepage = "github.com/grumply/pure-async";
  description = "";
  license = stdenv.lib.licenses.bsd3;
}
