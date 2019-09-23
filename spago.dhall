{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "bifunctors"
    , "console"
    , "debug"
    , "dotenv"
    , "effect"
    , "foreign-object"
    , "makkori"
    , "node-http"
    , "node-sqlite3"
    , "parsing"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
