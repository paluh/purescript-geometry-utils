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
    , "unordered-collections"
    ]
, packages = ../magusai/packages.dhall
}

