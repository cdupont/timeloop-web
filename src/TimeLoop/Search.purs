module TimeLoop.Search where

import TimeLoop.Types
import TimeLoop.Walker
import Data.Boolean
import Data.Foldable hiding (elem, all, foldr)
import Data.Eq
import Data.Maybe
import Data.Array
import Data.Traversable.Accum
import Control.Semigroupoid
import Data.Function
import Data.Semigroup
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Functor
import Data.Traversable hiding (elem, all, foldr)
import Control.Bind

-- Get all blocks that contains valid trajectories for the given universe
getValidSTBlocks :: Univ -> Array STBlock
getValidSTBlocks u = filter isValidBlock $ getAllSTBlocks u

getAllSTBlocks :: Univ -> Array STBlock
getAllSTBlocks u = map getSTBlock $ getPortalCombinations u.portals where
   getSTBlock :: Array Source -> STBlock
   getSTBlock scs = {univ : u,
                     walkers : join $ getAllWalkers $ getTimeline (scs <> u.emitters) (u.consumers <> map _.entry u.portals)}


-- A portal can emit a walker, or not.
-- We simulate all possible combinations of portal usage.
getPortalCombinations :: Array Portal -> Array (Array Source)
getPortalCombinations ps = subsequences $ map _.exit ps 

-- Get a list of Sources and Sinks, indexed by their time.
getTimeline :: Array Source -> Array Sink -> Array {sources :: Array Source, sinks :: Array Sink}
getTimeline emitters consumers = map getIOT (0..maxStep) where
  getIOT t = {sources : filter (\source -> t == source.time) emitters, 
              sinks : filter (\sink -> t == sink.time) consumers}

-- generate the full universe of walkers from a timeline. Arrays are indexed by Time.
getAllWalkers :: Array {sources :: Array Source, sinks :: Array Sink} -> Array (Array Walker)
getAllWalkers timeline = _.value $ mapAccumL getNextStep [] timeline

-- Move the walkers one step.
-- All walkers, sources, sinks should be from the same time frame. 
getNextStep :: Array Walker -> {sources :: Array Source, sinks :: Array Sink} -> Accum (Array Walker) (Array Walker)
getNextStep ws {sources, sinks} = 
  -- We move all walkers on step. New walkers appears on the sources. Walkers that are on a Sink are removed.
  -- This will be used by mapAccumL as input for the next step 
  --{ accum : (ws <> emitted) \\ consummed,  
  { accum : concatMap move $ posGroups $ (ws <> emitted) \\ consummed,  
--  -- We store the current walkers, together with the new walkers appearing at the sources.
--  -- This will be stored by mapAccumL in the final array
    value : ws <> emitted}
  where
  -- group the walkers that are on the same position
  posGroups :: Array Walker -> Array (Array Walker)
  posGroups as = map toArray $ groupBy ((==) `on` _.pos) $ sortWith (_.pos) as 
  consummed = sinks
  emitted = sources

-- A Universe is valid when a walker that enters a portal, also exits it. 
isValidBlock :: STBlock -> Boolean
isValidBlock {univ : {portals : ps}, walkers : ws} = all isValidPortal ps where
  isValidPortal {entry : sk, exit : sc} = (sc `elem` ws) == (sk `elem` ws)


-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- >>> subsequences "abc"
-- ["","a","b","ab","c","ac","bc","abc"]
subsequences :: forall a. Array a -> Array (Array a)
subsequences xs =  [] : nonEmptySubsequences xs

nonEmptySubsequences :: forall a. Array a -> Array (Array a)
nonEmptySubsequences xs = case uncons xs of 
  Just {head : x, tail : xs} -> [x] : foldr f [] (nonEmptySubsequences xs)
      where f ys r = ys : (x : ys) : r
  Nothing -> [] 
