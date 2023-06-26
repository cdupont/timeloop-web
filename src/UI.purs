
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
import Data.Array
import Data.Int
import Data.Traversable (sequence, traverse_)
import Graphics.Canvas (rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById, Context2D, fillRect)

import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes.Transform as SAT
import Record
import Debug
import Web.HTML.Common
import Effect.Console (logShow)
import Effect.Class (class MonadEffect)

type Item = {
  itemType :: ItemType,
  time :: Time,
  dir :: Dir,
  sel :: Maybe Boolean,  --selected item
  high :: Maybe Boolean, --item highlighted
  col  :: Maybe Int}     --item connected 

type ItemMap = M.Map Pos (Array Item)

type SelItem = {
  itemType  :: ItemType,
  itemIndex :: Int}

type Step = Int

type UI = {
  initUniv :: Univ,
  selItem  :: Maybe SelItem, -- Which item is selected
  stepItem :: Step,          -- A time step counter
  config   :: Config}   

type Config = {
  showSols :: Boolean,
  showWrongTrajs :: Boolean}


tileX = 36.0
tileY = 36.0

-- * Main app
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> UI
initialState _ = {initUniv: univ2, selItem: Just {itemType: EntryPortal, itemIndex: 0}, stepItem: 0, config: {showSols: false, showWrongTrajs: false}}

render :: forall w. UI -> HH.HTML w Action
render state =
    HH.div []
      [ HH.div [HP.class_ (ClassName "config")] --, HE.onClick \_ -> Test] 
          [ 
          drawBlock {univ: state.initUniv, walkers: []}
          ],
        HH.div [HP.class_ (ClassName "solutions")] 
          (map drawBlock $ (getAllSTBlocks state.initUniv))
      ]

data Action = Rotate Int

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM UI Action () output m Unit
handleAction a = case a of
  Rotate a -> do
     H.liftEffect $ logShow "test"
     H.modify_ \a -> a

           
drawBlock :: forall w i. STBlock -> HH.HTML w i
drawBlock block = HH.div [] $ singleton $ 
  SE.svg [SA.height 360.0, SA.width 360.0, SA.viewBox (toNumber lims.first.x) (toNumber lims.first.y) (toNumber lims.last.x) (toNumber lims.last.y)]
         [
           drawItemMap (getItemMap block Nothing Nothing) lims,
           SE.image [SA.x 0.0, SA.y 0.0, SA.width 9.0, SA.height 9.0, SA.href "assets/univ_background.svg"]
         ]

place :: forall w i. Pos -> HH.HTML w i -> HH.HTML w i
place {x, y} w = SE.g [SA.transform [SAT.Translate (toNumber x) (toNumber y)]] [w]


-- Draws items
drawItemMap :: forall w i. ItemMap -> Limits -> HH.HTML w i
drawItemMap is {first: {x: minX, y: minY}, last: {x: maxX, y: maxY}} = 
  trace (show is) \_ -> SE.g [] $ catMaybes $ concatMap row (range minY maxY) where
    row y = map (\x -> drawItems {x:x, y:y} is) (range minX maxX)

-- Draw items at a specific position
drawItems :: forall w i. Pos -> ItemMap -> Maybe (HH.HTML w i)
drawItems p is = case M.lookup p is of
  Just items -> Just $ place p $ drawTile items
  Nothing    -> Nothing

-- draw a single tile
-- Only the first item in the list will be displayed (except for collisions)
drawTile :: forall w i. Array Item -> HH.HTML w i
drawTile ai = case uncons ai of
  Just {head: {itemType: Walker_,     time: t1, dir: d1, sel, high, col}, tail: [{itemType: Walker_, time: t2, dir: d2}]} | t1 == t2  -> trace "here" \_-> setAttr sel high col $ getColTile d1 d2 t1                 
  Just {head: {itemType, time, dir, sel, high, col}, tail: _} -> trace "first" \_-> setAttr sel high col $ getTile itemType dir time 
  Nothing -> tileEmpty 

setAttr :: forall w i. Maybe Boolean -> Maybe Boolean -> Maybe Int -> HH.HTML i w -> HH.HTML i w 
setAttr sel high pair h = h --withDefAttr (pairAttr pair) . withDefAttr (selectAttr sel) . withDefAttr (dimAttr high) where


lims :: Limits
lims = {first: {x: 0, y: 0}, last: {x: 9, y: 9}}


getItemMap :: STBlock -> Maybe SelItem -> Maybe Step -> ItemMap
getItemMap u sel st = M.fromFoldableWith (<>) (addAttrs $ getItemMap' u)

addAttrs :: Array (Tuple Pos (Array {itemType:: ItemType, time:: Time, dir:: Dir})) -> Array (Tuple Pos (Array Item))
addAttrs ai = map (\(Tuple a b) -> (Tuple a (map (\c -> merge c {sel: Nothing, high: Nothing, col: Nothing}) b))) ai

-- Get the various items in Univ 
getItemMap' :: STBlock -> Array (Tuple Pos (Array {itemType:: ItemType, time:: Time, dir:: Dir})) --ItemMap
getItemMap' {univ: {portals, emitters, consumers}, walkers: walkers} = ems <> entries <> exits <> ws where
  ems = map (toTuple Exit) emitters
  cos = map (toTuple Entry) consumers
  exits = map (_.exit >>> toTuple ExitPortal ) portals
  entries = map (_.entry >>> toTuple EntryPortal) portals
  ws = map (toTuple Walker_) walkers
  toTuple it {pos, time, dir} = Tuple pos [{itemType: it, time, dir}]

 -- dimAttr (Just False) = dimA
 -- dimAttr _ = mempty
 -- selectAttr (Just True) = selA 
 -- selectAttr _ = mempty
 -- pairAttr (Just n) = portalA n
 -- pairAttr _ = mempty

---- * Events
--
--handleEvent  :: BrickEvent () Tick -> EventM () UI ()
--handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
--handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
--handleEvent (VtyEvent (V.EvKey V.KRight      [])) = modify $ move' E
--handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = modify $ move' W
--handleEvent (VtyEvent (V.EvKey V.KUp         [])) = modify $ move' N 
--handleEvent (VtyEvent (V.EvKey V.KDown       [])) = modify $ move' S 
--handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = modify rotate 
--handleEvent (VtyEvent (V.EvKey (V.KChar '+') [])) = modify $ changeTime True 
--handleEvent (VtyEvent (V.EvKey (V.KChar '-') [])) = modify $ changeTime False
--handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = modify changeItem
--handleEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = modify addPortal
--handleEvent (VtyEvent (V.EvKey (V.KChar 'e') [])) = modify addEmmiter
--handleEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = modify delItem 
--handleEvent (VtyEvent (V.EvKey V.KEnter      [])) = modify showSolutions 
--handleEvent (VtyEvent (V.EvKey (V.KChar 'w') [])) = modify showWrongTrajectories 
--handleEvent (VtyEvent (V.EvKey (V.KChar '1') [])) = modify $ solution univ1
--handleEvent (VtyEvent (V.EvKey (V.KChar '2') [])) = modify $ solution univ2
--handleEvent (VtyEvent (V.EvKey (V.KChar '3') [])) = modify $ solution univ3
--handleEvent (VtyEvent (V.EvKey (V.KChar '4') [])) = modify $ solution univ4
--handleEvent (VtyEvent (V.EvKey (V.KChar '5') [])) = modify $ solution univ5
--handleEvent (VtyEvent (V.EvKey (V.KChar '6') [])) = modify $ solution univ6
--handleEvent (VtyEvent (V.EvKey (V.KChar '7') [])) = modify $ solution univ7
--handleEvent (AppEvent Tick                      ) = modify increaseStep
--handleEvent _ = return ()
--
--
--solution :: Univ -> UI -> UI
--solution u ui = set #initUniv u ui
--
--move' :: Dir -> UI -> UI
--move' d = updateUI (movePos d)
--
--movePos :: Dir -> PTD -> PTD
--movePos N (PTD (Pos x y) t d) = PTD (Pos x (y+1)) t d 
--movePos S (PTD (Pos x y) t d) = PTD (Pos x (y-1)) t d 
--movePos E (PTD (Pos x y) t d) = PTD (Pos (x+1) y) t d 
--movePos W (PTD (Pos x y) t d) = PTD (Pos (x-1) y) t d 
--
--rotate :: UI -> UI
--rotate = updateUI (turn' Right_)
--
--changeTime :: Bool -> UI -> UI
--changeTime b = updateUI (changeTime' b)
--
--changeTime' :: Bool -> PTD -> PTD
--changeTime' True  (PTD p t d) = PTD p (t+1) d
--changeTime' False (PTD p t d) = PTD p (t-1) d
--
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
--
--updateUI :: (PTD -> PTD) -> UI -> UI
--updateUI f ui@(UI _ (Just (SelItem EntryPortal i)) _ _) = over (#initUniv % #portals % ix i % #entry % #unSink) f ui
--updateUI f ui@(UI _ (Just (SelItem ExitPortal i)) _ _)  = over (#initUniv % #portals % ix i % #exit % #unSource) f ui
--updateUI f ui@(UI _ (Just (SelItem Entry i)) _ _)       = over (#initUniv % #emitters % ix i % #unSource) f ui
--updateUI f ui@(UI _ (Just (SelItem Exit i)) _ _)        = over (#initUniv % #consumers % ix i % #unSink) f ui
--updateUI f ui = ui
--
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
