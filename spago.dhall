{ name = "test"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "console"
  , "css"
  , "debug"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "halogen-svg-elems"
  , "integers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "record"
  , "svg-parser-halogen"
  , "tuples"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
