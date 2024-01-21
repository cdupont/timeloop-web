
module Tile where

import Prelude
import TimeLoop.Types
import Halogen (ClassName(..))
import Halogen.HTML.Core (HTML)
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Transform as SAT
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Array (elem, fromFoldable, (:)) 
import Data.Maybe
import Data.Monoid (guard)
import Types
import Data.Int (toNumber)
import Web.UIEvent.MouseEvent as ME

tileX = 72.0
tileY = 72.0

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

timeAsset :: String
timeAsset = "assets/time.svg"

selAsset :: String
selAsset = "assets/sel.svg"


-- Get the tile with position, events and highlight
getTile :: forall w. Item -> HTML w Action 
getTile {itemType, itemIndex, pos, dirs, time, high, col, sel, top} = 
  SE.g 
    ( (SA.class_ $ ClassName $ "tile" <> (guard top " top")) :
      SA.transform [SAT.Translate (toNumber pos.x) (toNumber pos.y)] :
      guard (itemType `elem` selectable) [HE.onMouseDown $ \e -> StopPropagation (ME.toEvent e) $ Select $ Just {itemType, itemIndex}]
    )
    [ SE.svg 
        [ SA.height 1.0, 
          SA.width 1.0, 
          SA.viewBox 0.0 0.0 tileX tileY
        ]
        ( 
          [getTile' itemType time col dirs] <>
          guard sel [getAssetImage selAsset] <>
          guard high [getAssetImage timeAsset]
        )
    ]
         

-- Get the tile including collisions
getTile' :: forall w i. ItemType -> Maybe Time -> Color -> Array Dir -> HTML w i
getTile' Collision   t col ds  = SE.g [] $ map (\d -> getTile'' (asset Collision) col t (Just d)) ds
getTile' EntryPortal t col []  = getTile'' (asset EntryPortal) col t Nothing 
getTile' it          t col [d] = getTile'' (asset it) col t (Just d)
getTile' _           _ _   _   = SE.g [] []

-- Get the tile with its color, time index and orientation
getTile'' :: forall w i. String -> Color -> Maybe Time -> Maybe Dir -> HTML w i
getTile'' imAsset col time dir = 
  SE.g 
    [ SA.class_ $ getColorClass col ] 
    ( ( SE.g 
          [ SA.transform [SAT.Rotate (rotateDir (fromMaybe N dir)) (tileX / 2.0) (tileY / 2.0)] ]
          [ getAssetImage imAsset ] ) :
      ( getTime <$> fromFoldable time )
    )
                                    

getAssetImage :: forall w i. String -> HTML w i
getAssetImage s = SE.image [SA.x 0.0, SA.y 0.0, SA.width tileX, SA.height tileY, SA.href s]

getTime :: forall w i. Time -> HTML w i
getTime time = 
  SE.text 
    [ SA.x 23.0, SA.y 15.0 ]
    [ HH.text (show time) ]

