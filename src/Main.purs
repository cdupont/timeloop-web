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


data ItemType = EntryPortal | ExitPortal | Entry | Exit | Walker_

type Item = {
  itemType :: ItemType,
  time :: Time,
  dir :: Dir,
  sel :: Maybe Boolean,
  high :: Maybe Boolean,
  col  :: Maybe Int}

type ItemMap = M.Map Pos (Array Item)

type SelItem = {
  itemType  :: ItemType,
  itemIndex :: Int}

type Step = Int

type UI = {
  initUniv :: Univ,
  selItem  :: Maybe SelItem, -- Which item is selected
  stepItem :: Step,          -- A time step counter
  config   :: Config}   

type Config = {
  showSols :: Boolean,
  showWrongTrajs :: Boolean}

main :: Effect Unit
main = void $ unsafePartial do
  log $ show $ getValidSTBlocks univ2
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body


