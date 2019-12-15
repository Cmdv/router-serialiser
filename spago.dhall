{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "arrays"
    , "assert"
    , "console"
    , "contravariant"
    , "effect"
    , "generics-rep"
    , "globals"
    , "newtype"
    , "profunctor"
    , "psci-support"
    , "record"
    , "strings"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
