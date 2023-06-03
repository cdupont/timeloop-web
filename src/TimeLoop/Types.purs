

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

-- , Ord, Show, Enum, Bounded)

data RelDir = Front | Back | Right_ | Left_
--  deriving (Eq, Ord, Show, Enum, Bounded)

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

-- An Walker is a particle with a position, a time and a direction.
type Walker = PTD
--  deriving (Eq, Ord, Show, Generic)

type Source = PTD
--  deriving (Eq, Ord, Show, Generic)

type Sink = PTD
--  deriving (Eq, Ord, Show, Generic)

-- A portal links two points in space, at specific times and directions.
type Portal = {
  entry :: Sink,
  exit  :: Source}
--  deriving (Eq, Ord, Show, Generic)

-- A Univers contains some portals linking distant points in the spacetime block.
-- It also contains emitters and consumers which are point emitting or consuming one walker.
type Univ = {
  portals :: Array Portal,
  emitters :: Array Source,
  consumers :: Array Sink}
--  deriving (Eq, Ord, Show, Generic)

-- A STBlock is infinite and flat spacetime block universe.
-- It contains some "Walkers" which are particules that moves in a straight line.
type STBlock = {
  univ :: Univ,
  walkers :: Array Walker}
--  deriving (Eq, Show, Generic)

type Limits = { first :: Pos, last :: Pos}

maxStep :: Int
maxStep = 10

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
--No solution (self deviating)
univ1 :: Univ
univ1 = {portals : [{entry : {pos : {x : 6, y : 0}, time : 6, dir : E}, 
                     exit : {pos : {x : 3, y : 3}, time : 0, dir : S}}], 
         emitters : [{pos : {x : 0, y : 0}, time : 0, dir : E}], 
         consumers : []}

-- two solutions: going straight or going through portal
univ2 :: Univ
univ2 = {portals : [{entry : {pos : {x : 3, y : -3}, time : 6, dir : S},
                     exit : {pos : {x : 6, y : 0}, time : 0, dir : W}}],
         emitters : [{pos : {x : 0, y : 0}, time : 0, dir : E}],
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
