{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "geometry-utils"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "group"
  , "integers"
  , "math"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "test-unit"
  , "typelevel"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ../magusai/packages.dhall
}
