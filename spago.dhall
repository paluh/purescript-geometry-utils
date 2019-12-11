{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name = "geometry-utils"
, dependencies =
    [ "arrays"
    , "generics-rep"
    , "group"
    , "integers"
    , "math"
    , "prelude"
    , "typelevel"
    , "test-unit"
    ]
, packages = ../magusai/packages.dhall
}

