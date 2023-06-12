
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
import Data.Traversable (sequence, traverse_)
import Graphics.Canvas (rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById, Context2D, fillRect)

import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes.Transform as SAT

data ItemType = EntryPortal | ExitPortal | Entry | Exit | Walker_

type Item = {
  itemType :: ItemType,
  time :: Time,
  dir :: Dir,
  sel :: Maybe Boolean,
  high :: Maybe Boolean,
  col  :: Maybe Int}

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


--canvasComponent ::
--  forall query input output m.
--  MonadAff m =>
--  H.Component HH.HTML query input output m
--canvasComponent =
--  Hooks.component \_ _ -> Hooks.do
--    drawOnCanvas
--    Hooks.pure $ HH.canvas [ HP.id "canvas", HP.width 300, HP.height 300 ]
--
--newtype DrawOnCanvas hooks = DrawOnCanvas (UseEffect hooks)
--
--derive instance newtypeDrawOnCanvas :: Newtype (DrawOnCanvas hooks) _
--
--drawOnCanvas :: forall m. MonadAff m => Hook m DrawOnCanvas Unit
--drawOnCanvas =
--  Hooks.wrap Hooks.do
--    Hooks.captures {} Hooks.useTickEffect do
--      mcanvas <- liftEffect $ getCanvasElementById "canvas"
--      mcontext <- liftEffect $ sequence $ getContext2D <$> mcanvas
--      traverse_ drawRect mcontext
--      pure Nothing
--    Hooks.pure unit
--
--drawRect :: forall m. MonadAff m => Context2D -> m Unit
--drawRect context = do
--  liftEffect $ setFillStyle context "red"
--  liftEffect
--    $ fillRect context { x: 100.0, y: 50.0, width: 200.0, height: 25.0 }


tileX = 36.0

-- * Main app
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval --{ handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div 
      [ HP.id "svg-test-container" ]
      [ SE.svg 
        [SA.height 200.0, SA.width 200.0, SA.viewBox 0.0 0.0 300.0 300.0]
        [tileWalker N 1,
         SE.g [SA.transform [SAT.Translate (tileX * 2.0) 0.0]] [tilePortal true N 1],
         SE.g [SA.transform [SAT.Translate (tileX * 4.0) 0.0]] [tileCollision N E 1]
        ]
      ]


--  handleAction = undefined --case _ of
    --Increment -> H.modify_ \state -> state + 1
   -- Decrement -> H.modify_ \state -> state - 1

--app :: App UI Tick ()
--app = App
--  { appDraw         = drawUI
--  , appChooseCursor = neverShowCursor
--  , appHandleEvent  = handleEvent
--  , appStartEvent   = return ()
--  , appAttrMap      = theMap
--  }
--
--lims :: Limits
--lims = ((-1, -3), (7, 3))
--
--
---- * UI
--
---- Display the whole interface
--drawUI :: UI -> [Widget ()]
--drawUI (UI u sel st conf)= [center (drawConfigPanel u sel) <+> borderWithLabel (str "Instructions") (padAll 1 $ str help)
--                       <=> (str $ encouragement (showSols conf) (length $ getValidSTBlocks u))
--                       <=> (if showSols conf then drawSearchPanel u st conf else emptyWidget)]
--
---- Display the top panel for configuring the universe.
--drawConfigPanel :: Univ -> Maybe SelItem -> Widget ()
--drawConfigPanel u sel = borderWithLabel (str " Universe setup ") $ drawItemMap (getItemsUniv u sel Nothing) lims 
--
---- Display the various solutions
--drawSearchPanel :: Univ -> Step -> Config -> Widget ()
--drawSearchPanel u st conf = hBox $ if showWrongTrajs conf then showAllSols else showGoodSols where
--  showGoodSols = zipWith (\b i -> showSol b (" Solution n." ++ show i ++ " ") True st) (filter isValidBlock $ getAllSTBlocks u) [1..]
--  showAllSols = map (\b -> showSol b "" (isValidBlock b) st) $ getAllSTBlocks u
--
--showSol :: STBlock -> String -> Bool -> Step -> Widget ()
--showSol block msg isGood step = overrideAttr borderAttr (if isGood then borderGood else borderBad) $ drawBlock msg block step
--
--drawBlock :: String -> STBlock -> Step -> Widget ()
--drawBlock label block step = borderWithLabel (str label) $ drawItemMap (getItemMap block Nothing (Just step)) lims
--
---- Get the various items in a Block as a Map
--getItemMap :: STBlock -> Maybe SelItem -> Maybe Step -> ItemMap
--getItemMap (STBlock u ws) sel st = M.map (sortBy $ timePrio st) $ M.unionWith (++) (getItemsUniv u sel st) walkers where
--  walkers = M.fromListWith (++) $ map (\(Walker (PTD p t d)) -> (p, [Item Walker_ t d Nothing (highlighted t st) Nothing])) ws 
--
---- Get the various items in Univ 
--getItemsUniv :: Univ -> Maybe SelItem -> Maybe Step -> ItemMap
--getItemsUniv (Univ ps es cs) sel st = M.fromListWith (++) (entries ++ exits ++ portalEntries ++ portalExits) where
--  entries       = zipWith (\(Source (PTD p t d)) i            -> (p, [Item Entry       t d (selected sel Entry i)       (highlighted t st) Nothing])) es [0..]
--  exits         = zipWith (\(Sink (PTD p t d)) i              -> (p, [Item Exit        t d (selected sel Exit i)        (highlighted t st) Nothing])) cs [0..]
--  portalEntries = zipWith (\(Portal (Sink (PTD p t d)) _) i   -> (p, [Item EntryPortal t d (selected sel EntryPortal i) (highlighted t st) (Just i)])) ps [0..]
--  portalExits   = zipWith (\(Portal _ (Source (PTD p t d))) i -> (p, [Item ExitPortal  t d (selected sel ExitPortal i)  (highlighted t st) (Just i)])) ps [0..]
--
---- Highlight items that on the current timestep
--highlighted t (Just st') = Just $ (st' `div` 10 `mod` maxStep) == t
--highlighted _ _ = Nothing 
--
---- Items selected by the user
--selected :: Maybe SelItem -> ItemType -> Int -> Maybe Bool
--selected (Just (SelItem it index)) it' index' = Just ( it == it' && index == index')
--selected _ _ _ = Nothing 
--
---- Items that are on the current timestep will be displayed with higher priority.
--timePrio (Just st) (Item _ t1 _ _ _ _) _ | t1 == st `div` 10 `mod` maxStep = LT 
--timePrio (Just st) _ (Item _ t2 _ _ _ _) | t2 == st `div` 10 `mod` maxStep = GT 
--timePrio _ a b = compare a b
--
---- Draws items
--drawItemMap :: ItemMap -> Limits ->  Widget ()
--drawItemMap is ((minX, minY), (maxX, maxY)) = vBox $ map row [maxY, maxY-1 .. minY] where
--  row y = hBox $ map (\x -> drawItems (Pos x y) is) [minX..maxX]
--
---- Draw items at a specific position
--drawItems :: Pos -> ItemMap -> Widget ()
--drawItems p is = case M.lookup p is of
--  Just items -> drawTile items
--  Nothing    -> drawTile [] 
--
---- draw a single tile
---- Only the first item in the list will be displayed (except for collisions)
--drawTile :: [Item] -> Widget ()
--drawTile []                                                                             = str tileEmpty 
--drawTile ((Item EntryPortal t d sel high pair) : _)                                     = setAttr sel high pair $ str $ tilePortal True d t 
--drawTile ((Item ExitPortal t d sel high pair) : _)                                      = setAttr sel high pair $ str $ tilePortal False d t 
--drawTile ((Item Entry t d sel high pair) : _)                                           = setAttr sel high pair $ str $ tileEntry d t
--drawTile ((Item Exit t d sel high pair) : _)                                            = setAttr sel high pair $ str $ tileExit d t
--drawTile ((Item Walker_ t1 d1 sel high pair) : (Item Walker_ t2 d2 _ _ _) : _) | t1 == t2 = setAttr sel high pair $ str $ tileCollision d1 d2 t1
--drawTile ((Item Walker_ t d sel high pair) : _)                                          = setAttr sel high pair $ str $ tileWalker d t
--
--setAttr :: Maybe Bool -> Maybe Bool -> Maybe Int -> Widget () -> Widget () 
--setAttr sel high pair = withDefAttr (pairAttr pair) . withDefAttr (selectAttr sel) . withDefAttr (dimAttr high) where
--  dimAttr (Just False) = dimA
--  dimAttr _ = mempty
--  selectAttr (Just True) = selA 
--  selectAttr _ = mempty
--  pairAttr (Just n) = portalA n
--  pairAttr _ = mempty
--
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
