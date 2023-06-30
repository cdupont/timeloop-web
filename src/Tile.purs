
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


data ItemType = EntryPortal Dir Int
              | ExitPortal Dir Int
              | Entry Dir
              | Exit Dir 
              | Walker_ Dir
              | Collision Dir Dir

derive instance Generic ItemType _
derive instance Eq ItemType
derive instance Ord ItemType
instance i ∷ Show ItemType where
   show = genericShow 

rotateDir :: Dir -> Number
rotateDir N = 0.0
rotateDir E = 90.0
rotateDir S = 180.0
rotateDir W = 270.0

getTile :: forall w i. ItemType -> Time -> HTML w i
getTile it time = SE.svg [SA.height 1.0, SA.width 1.0, SA.viewBox 0.0 0.0 tileX tileY] 
                         [getTile' it,
                          getTime time]

getTile' :: forall w i. ItemType -> HTML w i
getTile' (EntryPortal dir link) = getAsset dir "assets/entry_portal.svg"
getTile' (ExitPortal dir link)  = getAsset dir "assets/exit_portal.svg"
getTile' (Entry dir)            = getAsset dir "assets/entry.svg"
getTile' (Exit dir)             = getAsset dir "assets/exit.svg"
getTile' (Walker_ dir)          = getAsset dir "assets/walker.svg"
getTile' (Collision d1 d2)      = SE.g [] 
                                       [getAsset d1 "assets/col.svg",
                                        getAsset d2 "assets/col.svg"]
                              
getAsset :: forall w i. Dir -> String -> HTML w i
getAsset dir asset = SE.g [SA.transform [SAT.Rotate (rotateDir dir) (tileX / 2.0) (tileY / 2.0)]] 
                          [SE.image [SA.x 0.0, SA.y 0.0, SA.width tileX, SA.height tileY, SA.href $ asset]]

getTime :: forall w i. Time -> HTML w i
getTime time = SE.text [SA.x 24.0, SA.y 12.0] [HH.text (show time)]

