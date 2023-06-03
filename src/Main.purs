module Main where

import Prelude
import Effect.Console (log)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById)
import Partial.Unsafe (unsafePartial)
import TimeLoop.Search
import TimeLoop.Types


main :: Effect Unit
main = void $ unsafePartial do
  log $ show $ getValidSTBlocks univ2
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  setFillStyle ctx "#00F"
  fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }
