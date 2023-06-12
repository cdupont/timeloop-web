
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
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
--import Data.Printf

tilePortal :: forall w i. Boolean -> Dir -> Time -> HTML w i
tilePortal in_ dir time = SE.svg [SA.height 36.0, SA.width 36.0, SA.viewBox 0.0 0.0 36.0 36.0] 
  ([SE.text [SA.x 12.0, SA.y 24.0] [HH.text (show time)]] <>
  [SE.text (tilePos side) [HH.text (showArr dir)]]) where 
     side = if in_ then turnRel Back dir else dir 

tileWalker :: forall w i. Dir -> Time -> HTML w i
tileWalker = tileArr showArr  

tileEntry :: forall w i. Dir -> Time -> HTML w i
tileEntry = tileArr showFromBarArr  

tileExit :: forall w i. Dir -> Time -> HTML w i
tileExit = tileArr showToBarArr  

tileArr :: forall w i. (Dir -> String) -> Dir -> Time -> HTML w i
tileArr showArr dir time = SE.svg [SA.height 36.0, SA.width 36.0, SA.viewBox 0.0 0.0 36.0 36.0] 
  [SE.text [SA.x 12.0, SA.y 24.0] [HH.text (showArr dir)],
   SE.text [SA.x 24.0, SA.y 12.0] [HH.text (show time)]
  ]

tileCollision :: forall w i. Dir -> Dir -> Time -> HTML w i
tileCollision d1 d2 time = SE.svg [SA.height 36.0, SA.width 36.0, SA.viewBox 0.0 0.0 36.0 36.0] 
   ([SE.text [SA.x 12.0, SA.y 24.0] [HH.text "★"]] <>
    [SE.text [SA.x 24.0, SA.y 12.0] [HH.text (show time)]] <> 
    [SE.text (tilePos d1) [HH.text (getAngleArr d1)]] <>
    [SE.text (tilePos d2) [HH.text (getAngleArr d2)]])

tilePos :: forall i. Dir -> Array (HP.IProp SI.SVGtext i)
tilePos E = [SA.x  0.0, SA.y 24.0]
tilePos S = [SA.x 12.0, SA.y 12.0]
tilePos W = [SA.x 24.0, SA.y 24.0]
tilePos N = [SA.x 12.0, SA.y 36.0]

showArr :: Dir -> String
showArr N = "↑"
showArr W = "←"
showArr E = "→"
showArr S = "↓"

showFromBarArr :: Dir -> String 
showFromBarArr N = "↥"
showFromBarArr W = "↤"
showFromBarArr E = "↦"
showFromBarArr S = "↧"

showToBarArr :: Dir -> String 
showToBarArr N = "⤒"
showToBarArr W = "⇤"
showToBarArr E = "⇥"
showToBarArr S = "⤓"

getAngleArr :: Dir -> String
getAngleArr N = "↱"
getAngleArr S = "↲"
getAngleArr E = "⬎"
getAngleArr W = "⬑"
