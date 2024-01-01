module Main where

import Prelude (Unit, bind, unit, void, ($))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import UI (component)


main :: Effect Unit
main = void $ unsafePartial do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

