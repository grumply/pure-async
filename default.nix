{ mkDerivation, base, pure-core, pure-default, stdenv }:
mkDerivation {
  pname = "pure-async";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-default ];
  homepage = "github.com/grumply/pure-async";
  description = "Async decorator";
  license = stdenv.lib.licenses.bsd3;
}
