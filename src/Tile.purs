
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
import CSS.Color
import CSS.Font
import Data.Show.Generic
import Data.Generic.Rep


--tilePortal :: forall w i. Boolean -> Dir -> Time -> HTML w i
--tilePortal in_ dir time = SE.svg [SA.height 36.0, SA.width 36.0, SA.viewBox 0.0 0.0 36.0 36.0] 
--  [SE.text [SA.x 12.0, SA.y 24.0] [HH.text (show time)],
--   SE.rect [SA.x 3.0, SA.y 3.0, SA.width 30.0, SA.height 30.0, SA.stroke (SA.Named "black"), SA.fill SA.NoColor],
--   SE.text (tilePos side) [HH.text (showArr dir)]
--  ] where 
--     side = if in_ then turnRel Back dir else dir 

tileX = 36.0
tileY = 36.0


data ItemType = EntryPortal | ExitPortal | Entry | Exit | Walker_
derive instance Generic ItemType _
instance i ∷ Show ItemType where
   show = genericShow 

tileAsset :: ItemType -> String
tileAsset EntryPortal = "assets/entry_portal.svg"
tileAsset ExitPortal  = "assets/exit_portal.svg"
tileAsset Entry       = "assets/entry.svg"
tileAsset Exit        = "assets/exit.svg"
tileAsset Walker_     = "assets/walker.svg"

colAsset :: String
colAsset = "assets/col.svg"

rotateDir :: Dir -> Number
rotateDir N = 0.0
rotateDir E = 90.0
rotateDir S = 180.0
rotateDir W = 270.0

getTile :: forall w i. ItemType -> Dir -> Time -> HTML w i
getTile it dir time = SE.svg [SA.height 1.0, SA.width 1.0, SA.viewBox 0.0 0.0 tileX tileY] 
                             [SE.g [SA.transform [SAT.Rotate (rotateDir dir) (tileX / 2.0) (tileY / 2.0)]] 
                                   [SE.image [SA.x 0.0, SA.y 0.0, SA.width tileX, SA.height tileY, SA.href $ tileAsset it]],
                              SE.text [SA.x 24.0, SA.y 12.0] [HH.text (show time)]]


getColTile :: forall w i. Dir -> Dir -> Time -> HTML w i
getColTile d1 d2 time = SE.svg [SA.height 1.0, SA.width 1.0, SA.viewBox 0.0 0.0 tileX tileY] 
                             [SE.g [SA.transform [SAT.Rotate (rotateDir d1) (tileX / 2.0) (tileY / 2.0)]] 
                                   [SE.image [SA.x 0.0, SA.y 0.0, SA.width tileX, SA.height tileY, SA.href $ colAsset]],
                              SE.g [SA.transform [SAT.Rotate (rotateDir d2) (tileX / 2.0) (tileY / 2.0)]] 
                                   [SE.image [SA.x 0.0, SA.y 0.0, SA.width tileX, SA.height tileY, SA.href $ colAsset]],
                              SE.text [SA.x 24.0, SA.y 12.0] [HH.text (show time)]]

tileEmpty :: forall w i. HTML w i
tileEmpty = SE.svg [SA.height 36.0, SA.width 36.0, SA.viewBox 0.0 0.0 36.0 36.0] 
  [SE.text [SA.x 12.0, SA.y 24.0] [HH.text "??"]
  ]

