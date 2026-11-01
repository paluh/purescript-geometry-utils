{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "geometry-utils"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "group"
  , "integers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "partial"
  , "prelude"
  , "test-unit"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ../magusai/packages.dhall
}
