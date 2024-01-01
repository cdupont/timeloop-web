
module Types where

import Prelude
import TimeLoop.Types
import Data.Array.NonEmpty as ANE
import Data.Show.Generic
import Data.Generic.Rep
import Data.Lens
import Data.Lens.Record
import Type.Proxy
import Data.Maybe
import Web.Event.Event (Event)

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

selectable = [EntryPortal, ExitPortal, Entry, Exit]

data Color = Black | Red | Blue

derive instance Generic Color _
instance j ∷ Show Color where
   show = genericShow 

type Item = {
  itemType :: ItemType,
  itemIndex :: Int,
  pos :: Pos,
  dirs :: Array Dir,
  time :: Maybe Time,
  high :: Boolean,  -- Highlighted when time is matching
  sel  :: Boolean,  -- Selected tile
  col  :: Color,    -- color of the tile
  top  :: Boolean}  -- tile is on top

type ItemMap = Array Item

type UI = {
  initUniv :: Univ,
  stepItem :: Time,          -- A time step counter
  selItem  :: Maybe SelItem, 
  config   :: Config}   

_initUniv :: forall a r. Lens' { initUniv :: a | r } a
_initUniv = prop (Proxy :: Proxy "initUniv")

type Config = {
  showSols :: Boolean,
  showWrongTrajs :: Boolean}

type SelItem = {
  itemType  :: ItemType,
  itemIndex :: Int}

data Action = Initialize
            | Select (Maybe SelItem)
            | Create Pos
            | Move Pos
            | MoveRel Dir
            | Delete
            | Rotate
            | ChangeTime Boolean
            | StopPropagation Event Action
            | Tick
            | Noop

