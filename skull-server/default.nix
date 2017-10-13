{ mkDerivation, aeson, base, base64-bytestring, bytestring
, case-insensitive, containers, data-default, diener, directory
, email-validate, entropy, gerippe, http-api-data, http-types, lens
, lifted-base, monad-control, monad-logger, mtl
, optparse-applicative, parsec, persistent, persistent-sqlite
, persistent-template, product-profunctors, purescript-bridge
, pwstore-fast, servant, servant-docs, servant-purescript
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
    aeson base base64-bytestring bytestring case-insensitive containers
    diener email-validate entropy gerippe http-api-data http-types lens
    lifted-base monad-control monad-logger mtl parsec persistent
    persistent-template product-profunctors pwstore-fast servant
    servant-docs servant-purescript servant-server text text-show time
    transformers transformers-base wai
  ];
  executableHaskellDepends = [
    aeson base bytestring containers data-default diener directory
    http-api-data http-types lens mtl optparse-applicative
    persistent-sqlite purescript-bridge servant servant-docs
    servant-purescript servant-server servant-subscriber text text-show
    warp
  ];
  homepage = "https://github.com/rubenmoor/skull";
  description = "game server for the simple game skull";
  license = stdenv.lib.licenses.mit;
}
