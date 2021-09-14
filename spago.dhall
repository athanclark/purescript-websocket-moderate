{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-websocket-moderate"
, dependencies =
  [ "argonaut"
  , "arraybuffer"
  , "exceptions"
  , "monad-control"
  , "nullable"
  , "text-encoding"
  , "web-file"
  , "arraybuffer-types"
  , "effect"
  , "either"
  , "foreign"
  , "maybe"
  , "prelude"
  , "profunctor"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/athanclark/purescript-websocket-moderate.git"
}
