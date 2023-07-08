
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

data Color = Black | Red | Blue

type Item = {
  itemType :: ItemType,
  itemIndex :: Int,
  pos :: Pos,
  dirs :: ANE.NonEmptyArray Dir,
  time :: Time,
  high :: Boolean,
  sel  :: Boolean,
  col :: Color}

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
            | Rotate SelItem 
            | ChangeTime SelItem Boolean
            | Move SelItem Dir
            | StopPropagation Event Action
            | Tick
            | Noop
