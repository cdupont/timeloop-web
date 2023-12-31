
module TimeLoop.Walker where

import Prelude
import TimeLoop.Types
import Data.Enum
import Data.Bounded
import Data.Maybe
import Data.Semiring
import Data.Ring
import Data.Functor
import Control.Semigroupoid

-- move one or several walkers that are at the same point in spacetime
-- in case of collision, we always turn right
move :: Array Walker -> Array Walker
move [w] = [simpleMove w]
move ws = map (turn Right_ >>> simpleMove) ws

-- Move one step in a flat universe.
simpleMove :: Walker -> Walker
simpleMove w = w { pos = simpleMove' w.dir w.pos, time = w.time + 1}

simpleMove' :: Dir -> Pos -> Pos
simpleMove' S p = p {y = p.y + 1}
simpleMove' N p = p {y = p.y - 1}
simpleMove' E p = p {x = p.x + 1}
simpleMove' W p = p {x = p.x - 1}


-- Turn a walker using a relative direction
turn :: RelDir -> Walker -> Walker
turn rd w =  turn' rd w

turn' :: RelDir -> PTD -> PTD 
turn' rd ptd = ptd { dir = turnRel rd ptd.dir }

-- Turn an absolute direction using a relative one
turnRel :: RelDir -> Dir -> Dir
turnRel Right_ = nextDir
turnRel Back   = nextDir >>> nextDir  
turnRel Left_  = nextDir >>> nextDir >>> nextDir 
turnRel Front  = identity 

nextDir :: Dir -> Dir
nextDir d = case succ d of
             Just d' -> d'
             Nothing -> bottom

relDirN :: Dir -> RelDir
relDirN N = Back
relDirN S = Front
relDirN E = Left_
relDirN W = Right_

relTurn :: Dir -> Dir -> RelDir
relTurn d1 d2 | turnRel Right_ d1 == d2 = Right_ 
relTurn d1 d2 | turnRel Left_ d1 == d2 = Left_ 
relTurn d1 d2 | turnRel Front d1 == d2 = Front 
relTurn d1 d2 | turnRel Back d1 == d2 = Back 
relTurn _ _ = Front 
