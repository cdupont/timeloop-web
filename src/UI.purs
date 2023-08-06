
module UI where

import Prelude
import TimeLoop.Types
import TimeLoop.Search
import TimeLoop.Walker
import Tile
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH 
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Halogen.Hooks (Hook, UseEffect)
import Halogen.Hooks as Hooks
import Data.Newtype (class Newtype)
import Data.Maybe
import Data.Map as M
import Data.Tuple (Tuple(..))
import Data.Array hiding (union)
import Data.Int
import Data.Traversable (sequence, traverse_)
import Graphics.Canvas (rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById, Context2D, fillRect)

import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes.Transform as SAT
import Halogen.HTML.Events as HE
import Record
import Debug
import Web.HTML.Common
import Effect.Console (logShow)
import Effect.Class (class MonadEffect)
import Undefined
import Data.Array.Partial as AP
import Data.Foldable as F
import Data.Function
import Data.Array.NonEmpty as AN
import Data.Profunctor.Strong
import Data.Eq
import Data.Ord
import Data.Array.NonEmpty as ANE
import Data.NonEmpty as NE
import Partial.Unsafe
import Halogen.Subscription as HS
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Rec.Class (forever)
import Data.EuclideanRing
import Types
import Data.Lens
import Data.Lens.Index
import Web.UIEvent.KeyboardEvent
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Halogen.Query.Event (eventListener)
import Web.Event.Event as E
import Web.UIEvent.MouseEvent as ME
import Web.CSSOM.MouseEvent as CSSME

-- * Main app

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
       { handleAction = handleAction,
         initialize = Just Initialize }
    }

initialState :: forall i. i -> UI
initialState _ = {initUniv: univ2, stepItem: 0, selItem: Nothing, config: {showSols: false, showWrongTrajs: false}, delayPortal: Nothing}

render :: forall w. UI -> HH.HTML w Action
render state =
    HH.div []
      [ HH.div [HP.class_ (ClassName "config")] 
               [drawBlock Nothing state.selItem {univ: state.initUniv, walkers: []}],
        HH.div [HP.class_ (ClassName "solutions")] 
               (map (drawBlock (Just state.stepItem) Nothing) (getValidSTBlocks state.initUniv))
      ]


           
drawBlock :: forall w. Maybe Time -> Maybe SelItem -> STBlock -> HH.HTML w Action
drawBlock mt sel block = HH.div [] $ singleton $ 
  SE.svg [SA.height 360.0, SA.width 360.0, SA.viewBox (toNumber lims.first.x) (toNumber lims.first.y) (toNumber lims.last.x) (toNumber lims.last.y)
          --, HE.onMouseDown \e -> DelayPortalStart {x: floor $ (toNumber $ CSSME.offsetX e) / tileX, y: floor $ (toNumber $ CSSME.offsetY e) / tileY}
          --, HE.onMouseUp   \e -> DelayPortalEnd   {x: floor $ (toNumber $ CSSME.offsetX e) / tileX, y: floor $ (toNumber $ CSSME.offsetY e) / tileY}
          ]
         [
           SE.image [SA.x 0.0, SA.y 0.0, SA.width 9.0, SA.height 9.0, SA.href "assets/univ_background.svg"],
           drawItemMap (getItemMap block mt sel) lims
         ]


-- Draws items
drawItemMap :: forall w. ItemMap -> Limits -> HH.HTML w Action
drawItemMap is {first: {x: minX, y: minY}, last: {x: maxX, y: maxY}} = SE.g [] $ map getTile is

lims :: Limits
lims = {first: {x: 0, y: 0}, last: {x: 9, y: 9}}


getItemMap :: STBlock -> Maybe Time -> Maybe SelItem -> ItemMap
getItemMap stb mt sel = selectTopTile mt $ getItemMap' stb mt sel

-- Get the various items in Univ 
getItemMap' :: STBlock -> Maybe Time -> Maybe SelItem -> Array Item 
getItemMap' {univ : {portals, emitters, consumers}, walkers : walkers} mt sel = ems <> ps <> ws where
  -- convert STBlock elements into items
  ems = zipWith (getItem Exit mt Black sel) emitters (0..10)
  -- cos = zipWith (getItem Entry mt Black sel) consumers (0..10)
  ps = concat $ zipWith (\{exit, entry} i -> [getItem ExitPortal  mt (toColor $ i+1) sel exit  i, 
                                              {itemType : EntryPortal, itemIndex : i, pos : entry, dirs : [], time : Nothing, high : false, col : (toColor $ i+1), sel : false, top : true}]) portals (0..10)
  ws = map col $ groupBy ((==) `on` (_.pos &&& _.time)) $ sortBy (comparing (_.pos &&& _.time)) $ walkers 
  -- Create collisions for walkers at the same pos and time
  col as = if ANE.length as == 1 
             then getItem' Walker_   mt Black Nothing as 0 
             else getItem' Collision mt Black Nothing as 0

getItem :: ItemType -> Maybe Time -> Color -> Maybe SelItem -> PTD -> Int -> Item
getItem  it mt c sel ptd i = getItem' it mt c sel (ANE.singleton ptd) i

getItem' :: ItemType -> Maybe Time -> Color -> Maybe SelItem -> ANE.NonEmptyArray PTD -> Int -> Item
getItem' it mt c sel ptds i = {itemType:  it, 
                               itemIndex: i, 
                               pos:       first.pos,
                               dirs:      ANE.toArray $ map _.dir ptds, 
                               time:      Just first.time, 
                               high:      Just first.time == mt,
                               col:       c,
                               sel:       sel == Just {itemType: it, itemIndex: i},
                               top:       true} where
  first = ANE.head ptds


selectTopTile ::  Maybe Time -> Array Item -> Array Item
selectTopTile mt is = map (\i -> i {top = isTop i}) is where
  isTop :: Item -> Boolean
  isTop i = F.and $ map (isTop' i) is
  isTop' :: Item -> Item -> Boolean
  isTop' i1 i2 = if (i1.pos == i2.pos)
                 then if (i1.time == mt) == (i2.time == mt)
                      then i1.itemType <= i2.itemType
                      else (i1.time == mt) >= (i2.time == mt)
                 else true



-- * Events
handleAction :: forall output m. MonadAff m => Action -> H.HalogenM UI Action () output m Unit
handleAction a = case a of
  Initialize -> do
    _ <- H.subscribe =<< timer Tick
    pure unit
  Select se ->            H.modify_ \state -> state {selItem = se}
  Rotate se ->            H.modify_ $ updateUI' se rotate
  ChangeTime se isPlus -> H.modify_ $ updateUI' se $ changeTime isPlus
  Move se d ->            H.modify_ $ updateUI' se $ movePos d
  Tick ->                 H.modify_ \state -> state {stepItem = (state.stepItem + 1) `mod` 10}
  Noop ->                 pure unit
  StopPropagation e cont -> do
     liftEffect $ E.preventDefault e
     liftEffect $ E.stopPropagation e
     handleAction cont
--  DelayPortalStart pos -> H.modify_ \state -> state {delayPortal = Just {pos1: pos, pos2: Nothing, delay: 5}}
--  DelayPortalEnd   pos -> H.modify_ \state -> state {delayPortal = Just {pos1: state.delayPortal.pos1, pos2: Just pos, delay: 5}}


timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter

--solution :: Univ -> UI -> UI
--solution u ui = set #initUniv u ui

movePos :: Dir -> PTD -> PTD
movePos N ptd = ptd {pos {y = ptd.pos.y -1}}  
movePos S ptd = ptd {pos {y = ptd.pos.y +1}}  
movePos E ptd = ptd {pos {x = ptd.pos.x +1}}  
movePos W ptd = ptd {pos {x = ptd.pos.x -1}}  

rotate :: PTD -> PTD
rotate = turn' Right_

changeTime :: Boolean -> PTD -> PTD
changeTime true  ptd  = ptd {time = ptd.time + 1}
changeTime false  ptd = ptd {time = ptd.time - 1}

--changeItem :: UI -> UI
--changeItem ui@(UI u s _ _) = ui {selItem = nextSel (getSels u) s} 
-- 
--nextSel :: (Eq a) => [a] -> Maybe a -> Maybe a
--nextSel [] _ = Nothing
--nextSel as Nothing = Just $ head as
--nextSel as (Just a) = case dropWhile (/=a) as of
--  [] -> Nothing
--  [_] -> Just $ head as
--  _:b:_ -> Just b where 
--
--getSels :: Univ -> [SelItem]
--getSels (Univ ps es cs) = portals ++ entries ++ exits where
--  portals = concatMap (\i -> [SelItem EntryPortal i, SelItem ExitPortal i]) [0..length ps-1]
--  entries = map (SelItem Entry) [0..length es-1]
--  exits = map (SelItem Exit) [0..length cs-1]
--
--addPortal :: UI -> UI
--addPortal ui = changeItem $ over (#initUniv % #portals) (++ [portal1]) ui
--
--addEmmiter :: UI -> UI
--addEmmiter ui = changeItem $ over (#initUniv % #emitters) (++ [initSource]) ui
--
--delItem :: UI -> UI
--delItem = changeItem . delItem'
--
--delItem' :: UI -> UI
--delItem' ui@(UI _ (Just (SelItem EntryPortal i)) _ _) = over (#initUniv % #portals) (deleteAt i) ui 
--delItem' ui@(UI _ (Just (SelItem ExitPortal i)) _ _) =  over (#initUniv % #portals) (deleteAt i) ui 
--delItem' ui@(UI _ (Just (SelItem Entry i)) _ _) =  over (#initUniv % #emitters) (deleteAt i) ui 
--delItem' ui@(UI _ (Just (SelItem Exit i)) _ _) =  over (#initUniv % #consumers) (deleteAt i) ui 
--delItem' ui = ui
--
--deleteAt i xs = ls ++ rs
--  where (ls, _:rs) = splitAt i xs

updateUI :: (PTD -> PTD) -> UI -> UI
updateUI f ui = case ui.selItem of
  Just sel -> updateUI' sel f ui
  Nothing -> ui 

updateUI' :: SelItem -> (PTD -> PTD) -> UI -> UI
updateUI' {itemType: EntryPortal, itemIndex: i} f = over (_initUniv <<< _portals <<< ix (spy "i" i) <<< _entry) (toPos f)
updateUI' {itemType: ExitPortal, itemIndex: i} f  = over (_initUniv <<< _portals <<< ix (spy "i" i) <<< _exit) f
updateUI' {itemType: Entry, itemIndex: i} f       = over (_initUniv <<< _consumers <<< ix (spy "i" i)) (toPos f) 
updateUI' {itemType: Exit, itemIndex: i} f        = over (_initUniv <<< _emitters <<< ix (spy "i" i)) f 
updateUI' _ _ = undefined

toPos :: (PTD -> PTD) -> Pos -> Pos 
toPos f p = _.pos $ f ({pos: p, time: 0, dir: N})

--increaseStep :: UI -> UI
--increaseStep (UI ps s st c)  = UI ps s (st+1) c
--
--showSolutions :: UI -> UI
--showSolutions = over (#config % #showSols) not 
--
--showWrongTrajectories :: UI -> UI
--showWrongTrajectories = over (#config % #showWrongTrajs) not 
--
---- * Attributes
--
--dimA, selA :: AttrName
--dimA   = attrName "Dim"
--selA   = attrName "Sel"
--portalA n = attrName $ "Portal" ++ show n
--borderGood = attrName "borderGood"
--borderBad = attrName "borderBad"
--
--portalColors :: [VA.Color]
--portalColors = [VA.yellow, VA.blue, VA.green]
--
--
--theMap :: UI -> AttrMap
--theMap (UI _ _ st _) = attrMap
--  V.defAttr $
--    [(dimA, VA.withStyle VA.defAttr VA.dim),
--     (borderGood, fg VA.green),
--     (borderBad, fg VA.red),
--     (selA, if even (st `div` 5) then VA.withStyle VA.defAttr VA.bold else VA.defAttr)] 
--   ++[ (portalA n, fg (portalColors !! n)) | n <- [0.. length portalColors - 1]] 
--
--
--help :: String
--help = "Keyboard arrows: move selected item\n" ++
--       "\'r\': rotate\n" ++
--       "\'+/-\': increase/decrease time\n" ++
--       "Space: change selected item\n" ++
--       "Enter: Show/Hide solutions\n" ++
--       "\'p\': add portal\n" ++
--       "\'e\': add emitter\n" ++
--       "\'d\': delete item\n" ++
--       "---------------------\n" ++
--       "Load examples:\n" ++
--       "\'1\': The Paradox\n" ++
--       "\'2\': Self-rightening solution\n" ++
--       "\'3\': The Djinn\n" ++
--       "\'4\': Djinn deviation\n" ++
--       "\'5\': The Northern Cross\n" ++
--       "\'6\': Kill one solution with Paradox\n" ++
--       "\'7\': 4 solutions\n"
--
--
--encouragement :: Bool -> Int -> String
--encouragement False _ = "Press Enter when you are ready."
--encouragement _ 0 = "No solutions! You've hit a paradox. Press \'w\' to see why."
--encouragement _ 1 = "There is only one possible trajectory."
--encouragement _ n = "There are " ++ show n ++ " possible trajectories."
