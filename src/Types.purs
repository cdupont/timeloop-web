
module Types where

import Prelude
import TimeLoop.Types
import Data.Array.NonEmpty as ANE
import Data.Show.Generic
import Data.Generic.Rep
import Data.Lens
import Data.Lens.Record
import Type.Proxy

data ItemType = EntryPortal
              | ExitPortal
              | Entry
              | Exit 
              | Walker_
              | Collision

derive instance Generic ItemType _
derive instance Eq ItemType
derive instance Ord ItemType
instance i ∷ Show ItemType where
   show = genericShow 

data Color = Black | Red | Blue

type Item = {
  itemType :: ItemType,
  itemIndex :: Int,
  pos :: Pos,
  dirs :: ANE.NonEmptyArray Dir,
  time :: Time,
  high :: Boolean,
  col :: Color}

type ItemMap = Array Item

type UI = {
  initUniv :: Univ,
  stepItem :: Time,          -- A time step counter
  config   :: Config}   

_initUniv :: forall a r. Lens' { initUniv :: a | r } a
_initUniv = prop (Proxy :: Proxy "initUniv")

type Config = {
  showSols :: Boolean,
  showWrongTrajs :: Boolean}

data Action = Initialize
            | Rotate ItemType Int
            | Tick

