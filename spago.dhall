{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "console"
    , "debug"
    , "dotenv"
    , "effect"
    , "foreign-object"
    , "makkori"
    , "node-http"
    , "node-sqlite3"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
