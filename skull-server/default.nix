{ mkDerivation, aeson, ansi-terminal, base, base64-bytestring
, bytestring, case-insensitive, containers, data-default
, data-ordlist, directory, email-validate, entropy, esqueleto
, fast-logger, http-api-data, http-types, lens, lifted-base
, monad-control, monad-logger, MonadRandom, mtl
, optparse-applicative, parsec, persistent, persistent-sqlite
, persistent-template, product-profunctors, purescript-bridge
, pwstore-fast, random, servant, servant-docs, servant-purescript
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
    aeson ansi-terminal base base64-bytestring bytestring
    case-insensitive containers data-default data-ordlist
    email-validate entropy esqueleto fast-logger http-api-data
    http-types lens lifted-base monad-control monad-logger MonadRandom
    mtl parsec persistent persistent-template product-profunctors
    pwstore-fast random servant servant-docs servant-purescript
    servant-server text text-show time transformers transformers-base
    wai
  ];
  executableHaskellDepends = [
    aeson base bytestring containers data-default directory
    http-api-data http-types lens monad-logger mtl optparse-applicative
    persistent-sqlite purescript-bridge servant servant-purescript
    servant-server servant-subscriber text text-show warp
  ];
  homepage = "https://github.com/rubenmoor/skull";
  description = "game server for the simple game skull";
  license = stdenv.lib.licenses.mit;
}
