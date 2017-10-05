{ mkDerivation, aeson, base, base64-bytestring, bytestring
, case-insensitive, containers, data-default, diener
, email-validate, entropy, http-api-data, http-types, lens
, lifted-base, monad-control, mtl, opaleye, optparse-applicative
, parsec, postgresql-simple, product-profunctors, purescript-bridge
, pwstore-fast, servant, servant-docs, servant-purescript_0_8_0_0
, servant-server, servant-subscriber, stdenv, text, text-show, time
, transformers, transformers-base, wai, warp
}:
mkDerivation {
  pname = "skull-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring case-insensitive diener
    email-validate entropy http-api-data http-types lens lifted-base
    monad-control mtl opaleye parsec postgresql-simple
    product-profunctors pwstore-fast servant servant-docs
    servant-purescript_0_8_0_0 servant-server text text-show time transformers
    transformers-base wai
  ];
  executableHaskellDepends = [
    aeson base bytestring containers data-default diener http-api-data
    http-types lens mtl optparse-applicative postgresql-simple
    purescript-bridge servant servant-docs servant-purescript_0_8_0_0
    servant-server servant-subscriber text text-show warp
  ];
  homepage = "https://github.com/rubenmoor/skull";
  description = "game server for the simple game skull";
  license = stdenv.lib.licenses.mit;
}
