{ mkDerivation, base, blaze-markup, bytestring, directory
, exceptions, filepath, http-conduit, monad-control, network
, network-uri, semigroups, stdenv, template-haskell, text
, transformers, utf8-string, wai
}:
mkDerivation {
  pname = "airbrake";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base blaze-markup bytestring directory exceptions filepath
    http-conduit monad-control network network-uri semigroups
    template-haskell text transformers utf8-string wai
  ];
  homepage = "https://github.com/joelteon/airbrake";
  description = "An Airbrake notifier for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
