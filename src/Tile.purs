
module Tile where

import Prelude
import TimeLoop.Types
import TimeLoop.Walker
import Halogen (ClassName(..), Component)
import Halogen.Svg.Elements
import Halogen.HTML.Core (HTML, ElemName(..), Namespace(..))
import Halogen.HTML.Elements (Node, Leaf, elementNS)
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Indexed as SI
import Halogen.Svg.Attributes.Transform as SAT
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import Halogen.HTML.CSS
import Halogen.HTML.Events as HE
import Halogen.Svg.Indexed as SI
import CSS.Color hiding (Color)
import CSS.Font
import Data.Show.Generic
import Data.Generic.Rep
import Data.Array.NonEmpty as ANE
import Data.Enum
import Data.Maybe
import Data.Monoid
import Types
import Data.Int
import Debug
import Web.UIEvent.MouseEvent
import Web.UIEvent.WheelEvent
import Web.UIEvent.KeyboardEvent as KBE

tileX = 36.0
tileY = 36.0

toColor :: Int -> Color
toColor 1 = Red
toColor 2 = Blue
toColor _ = Black


getColorClass :: Color -> ClassName
getColorClass Black = ClassName "black"
getColorClass Red   = ClassName "red"
getColorClass Blue  = ClassName "blue"

rotateDir :: Dir -> Number
rotateDir N = 0.0
rotateDir E = 90.0
rotateDir S = 180.0
rotateDir W = 270.0

asset :: ItemType -> String 
asset EntryPortal = "assets/entry_portal.svg"
asset ExitPortal  = "assets/exit_portal.svg" 
asset Entry       = "assets/entry.svg"       
asset Exit        = "assets/exit.svg"        
asset Walker_     = "assets/walker.svg"      
asset Collision   = "assets/col.svg"      

timeAsset = "assets/time.svg"
selAsset = "assets/sel.svg"

keyDown :: KBE.KeyboardEvent -> SelItem-> Action
keyDown ke se = case (KBE.key ke) of
  "ArrowUp"    -> Move se N
  "ArrowDown"  -> Move se S
  "ArrowRight" -> Move se E
  "ArrowLeft"  -> Move se W
  "r"          -> Rotate se
  _            -> Noop

-- Get the tile with position, events and highlight
getTile :: forall w. Item -> HTML w Action 
getTile {itemType, itemIndex, pos, dirs, time, high, col, sel} = 
  SE.g [SA.class_ $ ClassName "tile",
        SA.transform [SAT.Translate (toNumber pos.x) (toNumber pos.y)],
        HE.onClick \e -> Select $ Just {itemType, itemIndex},
        HE.onKeyDown \e -> StopPropagation (KBE.toEvent e) $ keyDown e {itemType, itemIndex},
        HP.tabIndex 0
        ] 
        [
          SE.svg [SA.height 1.0, SA.width 1.0, SA.viewBox 0.0 0.0 tileX tileY] $ 
                  (guard high [getAssetImage timeAsset]) <>
                  [getTile' itemType time col dirs] <>
                  (guard sel [getAssetImage selAsset])
        ]
         

-- Get the tile including collisions
getTile' :: forall w i. ItemType -> Time -> Color -> ANE.NonEmptyArray Dir -> HTML w i
getTile' Collision  t col ds = SE.g [] $ ANE.toArray $ map (getTile'' (asset Collision) col t) ds
getTile' it t col ds         = getTile'' (asset it) col t (ANE.head ds)

-- Get the tile with its color, time index and orientation
getTile'' :: forall w i. String -> Color -> Time -> Dir -> HTML w i
getTile'' asset col time dir = SE.g [SA.class_ $ getColorClass col] 
                                    [
                                      SE.g [SA.transform [SAT.Rotate (rotateDir dir) (tileX / 2.0) (tileY / 2.0)]]
                                           [getAssetImage asset],
                                      getTime time
                                    ]

getAssetImage :: forall w i. String -> HTML w i
getAssetImage s = SE.image [SA.x 0.0, SA.y 0.0, SA.width tileX, SA.height tileY, SA.href s]

getTime :: forall w i. Time -> HTML w i
getTime time = SE.text [SA.x 24.0, SA.y 12.0] [HH.text (show time)]

