{ mkDerivation, base, bytestring, foldl, managed
, neat-interpolation, optparse-applicative, stdenv, text, turtle
}:
mkDerivation {
  pname = "nix-delegate";
  version = "1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring foldl managed neat-interpolation
    optparse-applicative text turtle
  ];
  executableHaskellDepends = [ base ];
  description = "Convenient utility for distributed Nix builds";
  license = stdenv.lib.licenses.asl20;
}
