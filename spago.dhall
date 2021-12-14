{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "parsing-validation"
, dependencies = [ "either", "foldable-traversable", "maybe", "parsing", "prelude", "psci-support", "transformers", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/markfarrell/purescript-parsing-validation.git"
}
