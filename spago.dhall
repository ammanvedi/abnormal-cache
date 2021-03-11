{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "teller"
, dependencies =
  [ "argonaut"
  , "arrays"
  , "assert"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "folds"
  , "foreign"
  , "foreign-object"
  , "integers"
  , "js-date"
  , "lists"
  , "math"
  , "maybe"
  , "now"
  , "numbers"
  , "psci-support"
  , "spec"
  , "tuples"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/ammanvedi/teller"
}
