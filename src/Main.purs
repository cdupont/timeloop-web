module Main where

import Prelude
import Effect.Console (log)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Map as M
import Graphics.Canvas (rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById)
import Partial.Unsafe (unsafePartial)
import TimeLoop.Search
import TimeLoop.Types
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import UI


main :: Effect Unit
main = void $ unsafePartial do
  log $ show $ univ2
  log $ show $ getValidSTBlocks $ univ2
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body


