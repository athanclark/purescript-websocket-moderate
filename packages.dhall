let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210905/packages.dhall
        sha256:140f3630801f2b02d5f3a405d4872e0af317e4ef187016a6b00f97d59d6275c6

let overrides = {=}

let additions =
      { text-encoding =
        { dependencies =
          [ "arraybuffer-types"
          , "either"
          , "exceptions"
          , "functions"
          , "strings"
          ]
        , repo = "https://github.com/AlexaDeWit/purescript-text-encoding.git"
        , version = "v1.0.0"
        }
      }

in  upstream // overrides // additions
