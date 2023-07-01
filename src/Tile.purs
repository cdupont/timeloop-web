
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
import Data.Array.NonEmpty as ANE
import Data.Enum
import Data.Maybe

tileX = 36.0
tileY = 36.0


data ItemType = EntryPortal Dir Int
              | ExitPortal Dir Int
              | Entry Dir
              | Exit Dir 
              | Walker_ Dir
              | Collision (ANE.NonEmptyArray Dir)

derive instance Generic ItemType _
derive instance Eq ItemType
derive instance Ord ItemType
instance i ∷ Show ItemType where
   show = genericShow 

data Color = Black | Red | Blue
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

timeAsset = "assets/time.svg"

getTile :: forall w i. ItemType -> Time -> Boolean -> HTML w i
getTile it time high = SE.svg [SA.height 1.0, SA.width 1.0, SA.viewBox 0.0 0.0 tileX tileY] 
                              [
                               if high then getAsset' timeAsset else SE.text [] [HH.text ""],
                               getTile' it time                               ]

getTile' :: forall w i. ItemType -> Time -> HTML w i
getTile' (EntryPortal dir col) t = getAsset "assets/entry_portal.svg" col t dir
getTile' (ExitPortal dir col)  t = getAsset "assets/exit_portal.svg"  col t dir
getTile' (Entry dir)           t = getAsset "assets/entry.svg"        0   t dir
getTile' (Exit dir)            t = getAsset "assets/exit.svg"         0   t dir
getTile' (Walker_ dir)         t = getAsset "assets/walker.svg"       0   t dir
getTile' (Collision ds)        t = SE.g [] $ ANE.toArray $ map (getAsset "assets/col.svg" 0 t) ds
                              
getAsset :: forall w i. String -> Int -> Time -> Dir -> HTML w i
getAsset asset col time dir = SE.g [SA.class_ $ getColorClass $ toColor col] 
                                   [
                                     SE.g [SA.transform [SAT.Rotate (rotateDir dir) (tileX / 2.0) (tileY / 2.0)]]
                                          [getAsset' asset],
                                     getTime time
                                   ]

getAsset' :: forall w i. String -> HTML w i
getAsset' s = SE.image [SA.x 0.0, SA.y 0.0, SA.width tileX, SA.height tileY, SA.href s]

getTime :: forall w i. Time -> HTML w i
getTime time = SE.text [SA.x 24.0, SA.y 12.0] [HH.text (show time)]

