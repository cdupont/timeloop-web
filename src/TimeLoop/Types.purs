

module TimeLoop.Types where

import Prelude
import Data.Eq
import Data.Ord
import Data.Enum
import Data.Bounded
import Data.Enum.Generic
import Data.Bounded.Generic
import Data.Show
import Data.Show.Generic
import Data.Generic.Rep
import Data.Lens
import Data.Lens.Record
import Type.Proxy

data Dir = N | E | S | W 

derive instance Generic Dir _
derive instance Eq Dir
derive instance Ord Dir
instance _4_ ∷ Enum Dir where
   succ = genericSucc
   pred = genericPred 
instance _5_ ∷ Bounded Dir where
   top = genericTop
   bottom = genericBottom 
instance _10_ ∷ Show Dir where
   show = genericShow 

data RelDir = Front | Back | Right_ | Left_
derive instance Generic RelDir _
instance _11__ ∷ Show RelDir where
   show = genericShow 

type Time = Int

type Pos = {
  x :: Int,
  y :: Int}
--  deriving (Eq, Ord, Show, Generic)

type PTD = {
  pos  :: Pos,
  time :: Time,
  dir  :: Dir}
--  deriving (Eq, Ord, Show, Generic)

_pos :: forall a r. Lens' { pos :: a | r } a
_pos = prop (Proxy :: Proxy "pos")

-- An Walker is a particle with a position, a time and a direction.
type Walker = PTD
--  deriving (Eq, Ord, Show, Generic)

type Source = PTD
--  deriving (Eq, Ord, Show, Generic)

type Sink = Pos
--  deriving (Eq, Ord, Show, Generic)

-- A portal links two points in space, at specific times and directions.
type Portal = {
  entry :: Sink,
  exit  :: Source}
--  deriving (Eq, Ord, Show, Generic)

_entry :: forall a r. Lens' { entry :: a | r } a
_entry = prop (Proxy :: Proxy "entry")
_exit :: forall a r. Lens' { exit :: a | r } a
_exit = prop (Proxy :: Proxy "exit")

-- A Univers contains some portals linking distant points in the spacetime block.
-- It also contains emitters and consumers which are point emitting or consuming one walker.
type Univ = {
  portals :: Array Portal,
  emitters :: Array Source,
  consumers :: Array Sink}
--  deriving (Eq, Ord, Show, Generic)

_portals :: forall a r. Lens' { portals :: a | r } a
_portals = prop (Proxy :: Proxy "portals")
_emitters :: forall a r. Lens' { emitters :: a | r } a
_emitters = prop (Proxy :: Proxy "emitters")
_consumers :: forall a r. Lens' { consumers :: a | r } a
_consumers = prop (Proxy :: Proxy "consumers")

data UObject = Portal | Source | Sink

-- A STBlock is infinite and flat spacetime block universe.
-- It contains some "Walkers" which are particules that moves in a straight line.
type STBlock = {
  univ :: Univ,
  walkers :: Array Walker}
--  deriving (Eq, Show, Generic)

type Limits = { first :: Pos, last :: Pos}

maxStep :: Int
maxStep = 12

--  sample data *

initSource :: Source
initSource = { pos: {x:0, y:0}, time:0, dir:E}
--
--source1 :: Source
--source1 = Source (PTD (Pos 1 (-1)) 0 N)
--
--source2 :: Source
--source2 = Source (PTD (Pos 0 3) 0 E)
--
--sink1 :: Sink
--sink1 = Sink (PTD (Pos 5 0) 5 E)
--
--walker1 :: Walker 
--walker1 = Walker (PTD (Pos 0 0) 0 E)
--
--walker2 :: Walker 
--walker2 = Walker (PTD (Pos 0 0) 0 N)
--
--walker3 :: Walker 
--walker3 = Walker (PTD (Pos 5 5) 0 N)
--
--portal1 :: Portal
--portal1 = Portal (Sink (PTD (Pos 0 0) 0 S)) (Source (PTD (Pos 1 0) 1 W))
--
univ0 :: Univ
univ0 = {portals : [], 
         emitters : [{pos : {x : 1, y : 3}, time : 0, dir : E}], 
         consumers : []}

--No solution (self deviating)
univ1 :: Univ
univ1 = {portals : [{entry : {x : 6, y : 0}, 
                     exit : {pos : {x : 3, y : 3}, time : 0, dir : S}}], 
         emitters : [{pos : {x : 0, y : 0}, time : 0, dir : E}], 
         consumers : []}

-- two solutions: going straight or going through portal
univ2 :: Univ
univ2 = {portals : [{entry : {x : 4, y : 0},
                     exit : {pos : {x : 7, y : 3}, time : 0, dir : W}}],
         emitters : [{pos : {x : 1, y : 3}, time : 0, dir : E}],
         consumers : []}

---- The Djinn
--univ3 :: Univ
--univ3 = Univ {portals = [Portal {entry = Sink {unSink = PTD {pos = Pos {x = 6, y = 0}, time = 6, dir = E}}, exit = Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}}], emitters = [], consumers = []}
--
----One solution: Deviate a Djinn 
--univ4 :: Univ
--univ4 = Univ {portals = [Portal {entry = Sink {unSink = PTD {pos = Pos {x = 6, y = 0}, time = 6, dir = E}}, exit = Source {unSource = PTD {pos = Pos {x = 3, y = -3}, time = 0, dir = N}}}], emitters = [Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}], consumers = []}
--
---- The northern cross
--univ5 :: Univ
--univ5 = Univ {portals = [Portal {entry = Sink {unSink = PTD {pos = Pos {x = 3, y = -3}, time = 6, dir = S}}, exit = Source {unSource = PTD {pos = Pos {x = 6, y = 0}, time = 0, dir = W}}},Portal {entry = Sink {unSink = PTD {pos = Pos {x = 3, y = 3}, time = 6, dir = N}}, exit = Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}}], emitters = [], consumers = []}
--
---- Kill one solution
--univ6 :: Univ
--univ6 = Univ {portals = [Portal {entry = Sink {unSink = PTD {pos = Pos {x = 3, y = -3}, time = 6, dir = S}}, exit = Source {unSource = PTD {pos = Pos {x = 6, y = 0}, time = 0, dir = W}}},Portal {entry = Sink {unSink = PTD {pos = Pos {x = 3, y = 3}, time = 6, dir = N}}, exit = Source {unSource = PTD {pos = Pos {x = 1, y = 2}, time = 3, dir = E}}}], emitters = [Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}], consumers = []}
--
---- 4 solutions
--univ7 :: Univ
--univ7 = Univ {portals = [Portal {entry = Sink {unSink = PTD {pos = Pos {x = 3, y = -3}, time = 4, dir = S}}, exit = Source {unSource = PTD {pos = Pos {x = 5, y = -1}, time = 0, dir = W}}},Portal {entry = Sink {unSink = PTD {pos = Pos {x = 5, y = 1}, time = 6, dir = E}}, exit = Source {unSource = PTD {pos = Pos {x = 1, y = 1}, time = 2, dir = E}}}], emitters = [Source {unSource = PTD {pos = Pos {x = 1, y = -1}, time = 0, dir = E}}], consumers = []}
--
