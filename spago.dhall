{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "bifunctors"
    , "console"
    , "debug"
    , "dotenv"
    , "effect"
    , "foreign-object"
    , "makkori"
    , "media-types"
    , "node-http"
    , "node-sqlite3"
    , "parsing"
    , "psci-support"
    , "validation"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
