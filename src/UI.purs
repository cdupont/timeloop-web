
module UI where

import Prelude
import Tile
import Halogen as H
import Halogen.HTML as HH 
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Halogen.Subscription as HS
import Halogen.Query.Event (eventListener)

import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.HTML.Common (ClassName(..))
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.WheelEvent.EventTypes as WET
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.MouseEvent as ME
import Web.Event.Event as E
import Web.CSSOM.MouseEvent as CSSME

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array hiding (union)
import Data.Int (floor, toNumber)
import Data.Foldable as F
import Data.Function (on)
import Data.Profunctor.Strong ((&&&))
import Data.Array.NonEmpty as ANE
import Data.Lens (over)
import Data.Lens.Index (ix)
import Data.Enum (upFrom)
import Effect.Class (liftEffect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Control.Monad.Rec.Class (forever)
import Debug (spy, trace) 
import Undefined (undefined)

import TimeLoop.Types
import TimeLoop.Search
import TimeLoop.Walker 
import Types

-- * Main app

component :: forall query input output m. MonadAff m => H.Component query input output m 
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
       { handleAction = handleAction,
         initialize = Just Initialize }
    }

initialState :: forall i. i -> UI
initialState _ = {initUniv: univ0, stepItem: 0, selItem: Nothing, config: {showSols: false, showWrongTrajs: false}, active: 1}

-- Render the full UI
render :: forall w. UI -> HH.HTML w Action
render ui =
    HH.div [HP.class_ (ClassName "game")] 
      [ 
        HH.div [HP.class_ (ClassName "solutions")] 
               ([HH.text msg] <>
               zipWith (\b i -> drawBlock (Just ui.stepItem) ui.selItem (i == ui.active) false (isValidBlock b) b i) blocks (1..10)),
        HH.div [HP.class_ (ClassName "play")] 
               (case blocks !! (ui.active-1) of
                  Nothing -> [drawBlock Nothing            ui.selItem false true true init_univ 0]
                  Just b ->  [drawBlock (Just ui.stepItem) ui.selItem false true true b         ui.active])
      ] where
        blocks = getAllSTBlocks $ ui.initUniv
        init_univ = {univ: ui.initUniv, walkers: []}
        msg = if length blocks == 0 then "No Solutions" else (show $ length blocks) <> " Solution(s):" 


-- Draw a single universe block.
drawBlock :: forall w. Maybe Time    -- A stepper time, allowing to highlight items
                    -> Maybe SelItem -- Item currently selected
                    -> Boolean       -- Universe is active
                    -> Boolean       -- Universe is playable
                    -> Boolean
                    -> STBlock       -- the universe block to display
                    -> Int           -- universe index
                    -> HH.HTML w Action
drawBlock mt sel active play isValid block i = 
  HH.div [HP.class_ (ClassName $ "solution" <> (if active then " active" else "") <> (if isValid then " valid" else " invalid")), 
          HP.id (solId i)]
         [ SE.svg [SA.height 864.0,
                   SA.width 864.0, 
                   SA.viewBox (toNumber lims.first.x) (toNumber lims.first.y) (toNumber lims.last.x) (toNumber lims.last.y), 
                   HE.onMouseDown $ \e -> StopPropagation (ME.toEvent e) $ if play then Create (getPos e) else SelectSol i,
                   HE.onMouseMove $ \e -> StopPropagation (ME.toEvent e) $ mouseMove $ spy "Mouse" e
                  ]
                  [
--                    SE.image [SA.x 0.0, SA.y 0.0, SA.width 11.0, SA.height 11.0, SA.href "assets/univ_background.svg"],
                    drawItemMap (getItemMap block mt sel) lims
                  ]
         ]

solId :: Int -> String
solId 0 = "init_univ"
solId i = "solution" <> (show i)

mouseMove :: ME.MouseEvent -> Action
mouseMove me = 
  if (ME.buttons me) == 1 
    then Move $ getPos me
    else Noop 


getPos :: ME.MouseEvent -> Pos
getPos e = {x: floor $ (toNumber $ CSSME.offsetX e) / tileX - 0.5, 
            y: floor $ (toNumber $ CSSME.offsetY e) / tileY - 0.5}


-- Draws items
drawItemMap :: forall w. ItemMap -> Limits -> HH.HTML w Action
drawItemMap is {first: {x: minX, y: minY}, last: {x: maxX, y: maxY}} = SE.g [] $ map getTile is

lims :: Limits
lims = {first: {x: 0, y: 0}, last: {x: 11, y: 11}}


-- Get the various items in Univ 
getItemMap :: STBlock -> Maybe Time -> Maybe SelItem -> ItemMap
getItemMap stb mt sel = selectTopTile mt $ getItemMap' stb mt sel

-- Get the various items in Univ 
getItemMap' :: STBlock -> Maybe Time -> Maybe SelItem -> Array Item 
getItemMap' {univ : {portals, emitters}, walkers : walkers} mt sel = 
  (getEmitterItems emitters mt sel) <> 
  (getPortalItems portals mt sel) <> 
  (getWalkerItems walkers mt)


--Get specific items  
getEmitterItems :: Array Source -> Maybe Time -> Maybe SelItem -> Array Item
getEmitterItems emitters mt sel = zipWith getE emitters (0..10) where
  getE exit i = {itemType : Exit, 
                 itemIndex : i, 
                 pos : exit.pos, 
                 dirs : [exit.dir], 
                 time : Just $ exit.time, 
                 high : (Just $ exit.time) == mt, 
                 col : Black, 
                 sel : sel == Just {itemType: Exit, itemIndex: i}, 
                 top : true}

getPortalItems :: Array Portal -> Maybe Time -> Maybe SelItem -> Array Item
getPortalItems portals mt sel = concat $ zipWith getP portals (0..10) where
  getP {exit, entry} i = [{itemType : ExitPortal, 
                           itemIndex : i, 
                           pos : exit.pos, 
                           dirs : [exit.dir], 
                           time : Just $ exit.time, 
                           high : (Just $ exit.time) == mt, 
                           col : (toColor $ i+1), 
                           sel : sel == Just {itemType: ExitPortal, itemIndex: i}, 
                           top : true},
                          {itemType : EntryPortal, 
                           itemIndex : i, 
                           pos : entry, 
                           dirs : [], 
                           time : Nothing, 
                           high : false, 
                           col : (toColor $ i+1), 
                           sel : sel == Just {itemType: EntryPortal, itemIndex: i}, 
                           top : true}
                          ]

getWalkerItems :: Array Walker -> Maybe Time -> Array Item
getWalkerItems walkers mt = map getW wsGroups where
  -- Group walkers by time and pos 
  wsGroups = groupBy ((==) `on` (_.pos &&& _.time)) $ sortBy (comparing (_.pos &&& _.time)) $ walkers 
  -- Create collisions for walkers at the same pos and time
  getW as = if ANE.length as == 1 
              then getW' Walker_   as
              else getW' Collision as
  getW' it ptds = {itemType:  it, 
                  itemIndex: 0, 
                  pos: _.pos $ ANE.head ptds, 
                  dirs: ANE.toArray $ map _.dir ptds, 
                  time: Just $ _.time $ ANE.head ptds, 
                  high: (Just $ _.time $ ANE.head ptds) == mt, 
                  col: Black, 
                  sel: false,
                  top: true} 


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



-- *** Events ***


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM UI Action () output m Unit
handleAction a = case a of
  Initialize -> do
    _ <- H.subscribe =<< timer Tick
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \_ -> eventListener KET.keyup (HTMLDocument.toEventTarget document) keyEvent
    H.subscribe' \_ -> eventListener WET.wheel (HTMLDocument.toEventTarget document) wheelEvent
    pure unit
  SelectSol i ->          trace ("Selectsol" <> show i) \_ -> H.modify_ \ui -> ui {active = i}
  Select se ->            trace ("Select" <> show se) \_ -> H.modify_ \ui -> ui {selItem = se}
  Tick ->                 H.modify_ \ui -> ui {stepItem = (ui.stepItem + 1) `mod` 10}
  Noop ->                 pure unit
  StopPropagation e cont -> do
     liftEffect $ E.preventDefault e
     liftEffect $ E.stopPropagation e
     handleAction cont
  Rotate            ->  H.modify_ $ updateUI $ \ptd -> ptd {dir = turnRel Right_ ptd.dir}
  ChangeTime isPlus ->  H.modify_ $ updateUI $ \ptd -> ptd {time =  if isPlus then ptd.time + 1 else ptd.time - 1}
  Delete        -> H.modify_ $ delItem 
  Create      p -> H.modify_ $ createPortal p 
  Move        p -> H.modify_ $ updateUI $ \ptd -> ptd {pos = p} 
  MoveRel     d -> H.modify_ $ updateUI $ \ptd -> ptd {pos = simpleMove' d ptd.pos} 
  ShowWrongTraj -> H.modify_ \ui -> ui {config {showWrongTrajs = not ui.config.showWrongTrajs}}

keyEvent :: E.Event -> Maybe Action
keyEvent e = case (spy "key: " $ KE.key <$> KE.fromEvent e) of
  Just "r"     -> Just $ Rotate
  Just "+"     -> Just $ ChangeTime true
  Just "-"     -> Just $ ChangeTime false
  Just "d"     -> Just $ Delete
  Just "Delete"     -> Just $ Delete
  Just "Backspace"  -> Just $ Delete
  Just "ArrowRight" -> Just $ MoveRel E 
  Just "ArrowLeft"  -> Just $ MoveRel W
  Just "ArrowUp"    -> Just $ MoveRel N 
  Just "ArrowDown"  -> Just $ MoveRel S 
  Just "w"          -> Just $ ShowWrongTraj 
  _            -> Nothing

wheelEvent :: E.Event -> Maybe Action
wheelEvent e = case (spy "key: " $ WE.deltaY <$> WE.fromEvent e) of
  Just w | w > 0.0 -> Just $ StopPropagation e $ ChangeTime true
  Just w | w < 0.0 -> Just $ StopPropagation e $ ChangeTime false
  _            -> Nothing
 

createPortal :: Pos -> UI -> UI
createPortal p ui = ui {selItem = Just {itemType: ExitPortal, itemIndex: length ui.initUniv.portals}, 
                        initUniv {portals = ui.initUniv.portals `snoc` {entry: p, exit: {pos: p, time: 0, dir: N}}}}


timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Aff.Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter


updateUI :: (PTD -> PTD) -> UI -> UI
updateUI f ui = case ui.selItem of
  Just sel -> updateUI' sel f ui
  Nothing -> ui 

updateUI' :: SelItem -> (PTD -> PTD) -> UI -> UI
updateUI' {itemType: EntryPortal, itemIndex: i} f = over (_initUniv <<< _portals <<< ix i <<< _entry) (toPos f)
updateUI' {itemType: ExitPortal, itemIndex: i} f  = over (_initUniv <<< _portals <<< ix i <<< _exit) f
updateUI' {itemType: Entry, itemIndex: i} f       = over (_initUniv <<< _consumers <<< ix i) (toPos f) 
updateUI' {itemType: Exit, itemIndex: i} f        = over (_initUniv <<< _emitters <<< ix i) f 
updateUI' _ _ = undefined

delItem :: UI -> UI
delItem ui = case ui.selItem of
  Just sel -> delItem' sel ui 
  Nothing -> ui 

delItem' :: SelItem -> UI -> UI
delItem' {itemType: EntryPortal, itemIndex: i} ui = ui { initUniv { portals = fromMaybe undefined (deleteAt i ui.initUniv.portals)}} 
delItem' {itemType: ExitPortal, itemIndex: i} ui  = ui { initUniv { portals = fromMaybe undefined (deleteAt i ui.initUniv.portals)}} 
delItem' {itemType: Entry, itemIndex: i} ui       = ui { initUniv { consumers = fromMaybe undefined (deleteAt i ui.initUniv.consumers)}} 
delItem' {itemType: Exit, itemIndex: i} ui        = ui { initUniv { emitters = fromMaybe undefined (deleteAt i ui.initUniv.emitters)}} 
delItem' _ _ = undefined

toPos :: (PTD -> PTD) -> Pos -> Pos 
toPos f p = _.pos $ f ({pos: p, time: 0, dir: N})



