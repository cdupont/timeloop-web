{ name = "test"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "halogen"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "tailrec"
  , "undefined"
  , "web-cssom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
