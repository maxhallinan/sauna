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
    , "datetime"
    , "debug"
    , "dotenv"
    , "effect"
    , "foreign-object"
    , "formatters"
    , "http-methods"
    , "makkori"
    , "media-types"
    , "node-http"
    , "node-sqlite3"
    , "node-url"
    , "now"
    , "nullable"
    , "parsing"
    , "psci-support"
    , "validation"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
