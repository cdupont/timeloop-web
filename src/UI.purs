
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
import Record hiding (set)
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
import Web.UIEvent.WheelEvent.EventTypes as WET
import Web.UIEvent.WheelEvent as WE

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
initialState _ = {initUniv: univ0, stepItem: 0, selItem: Nothing, config: {showSols: false, showWrongTrajs: false}, partialPortal: Just {entry: {x: 0, y:0}, exit: Nothing}, mode : AddMode}

render :: forall w. UI -> HH.HTML w Action
render ui =
    HH.div []
      [ 
        HH.div [HP.class_ (ClassName "solutions")] 
               (if length blocks == 0 
                 then [drawBlock Nothing ui.selItem {univ: getUniv ui, walkers: []}]
                 else map (drawBlock (Just ui.stepItem) Nothing) blocks)
      ] where
        blocks = getValidSTBlocks $ getUniv ui


           
drawBlock :: forall w. Maybe Time -> Maybe SelItem -> STBlock -> HH.HTML w Action
drawBlock mt sel block = HH.div [] $ singleton $ 
  SE.svg [SA.height 432.0, SA.width 432.0, SA.viewBox (toNumber lims.first.x) (toNumber lims.first.y) (toNumber lims.last.x) (toNumber lims.last.y)
          , HE.onMouseDown $ \e -> StopPropagation (ME.toEvent e) $ Create $ getPos e
          , HE.onMouseMove $ \e -> StopPropagation (ME.toEvent e) $ mouseMove e
          ]
         [
           SE.image [SA.x 0.0, SA.y 0.0, SA.width 11.0, SA.height 11.0, SA.href "assets/univ_background.svg"],
           drawItemMap (getItemMap block mt sel) lims
         ]

mouseMove :: ME.MouseEvent -> Action
mouseMove me = 
  if (ME.buttons me) == 1 
    then Move $ getPos me
    else Noop 

getUniv :: UI -> Univ
getUniv {initUniv, partialPortal : Nothing}                        = initUniv
getUniv {initUniv, partialPortal : Just {entry, exit : Nothing}}   = initUniv {consumers = entry : initUniv.consumers}
getUniv {initUniv, partialPortal : Just {entry, exit : Just exit}} = initUniv {portals = {entry : entry, exit : exit} : initUniv.portals}

getPos :: ME.MouseEvent -> Pos
getPos e = {x: floor $ (toNumber $ CSSME.offsetX e) / tileX - 0.5, 
            y: floor $ (toNumber $ CSSME.offsetY e) / tileY - 0.5}


-- Draws items
drawItemMap :: forall w. ItemMap -> Limits -> HH.HTML w Action
drawItemMap is {first: {x: minX, y: minY}, last: {x: maxX, y: maxY}} = SE.g [] $ map getTile is

lims :: Limits
lims = {first: {x: 0, y: 0}, last: {x: 11, y: 11}}


getItemMap :: STBlock -> Maybe Time -> Maybe SelItem -> ItemMap
getItemMap stb mt sel = selectTopTile mt $ getItemMap' stb mt sel

-- Get the various items in Univ 
getItemMap' :: STBlock -> Maybe Time -> Maybe SelItem -> Array Item 
getItemMap' {univ : {portals, emitters, consumers}, walkers : walkers} mt sel = ems <> cos <> ps <> ws where
  -- convert STBlock elements into items
  ems = zipWith (getItem Exit mt Black sel) emitters (0..10)
  cos = zipWith (\pos i -> {itemType : EntryPortal, itemIndex : i, pos : pos, dirs : [], time : Nothing, high : false, col : (toColor $ i+1), sel : false, top : true}) consumers (0..10)
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
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        keyEvent
    H.subscribe' \sid ->
      eventListener
        WET.wheel
        (HTMLDocument.toEventTarget document)
        wheelEvent
    pure unit
  Select se ->            H.modify_ \ui -> ui {selItem = se}
  Tick ->                 H.modify_ \ui -> ui {stepItem = (ui.stepItem + 1) `mod` 10}
  Noop ->                 pure unit
  StopPropagation e cont -> do
     liftEffect $ E.preventDefault e
     liftEffect $ E.stopPropagation e
     handleAction cont
  Mode m        -> H.modify_ $ mode m
  Rotate            ->  H.modify_ $ rotate
  ChangeTime isPlus ->  H.modify_ $ changeTime isPlus
  Delete        -> H.modify_ $ identity
  Create      p -> undefined 
  Move        p -> H.modify_ $ updateUI $ \ptd -> ptd {pos = p} 

keyEvent :: E.Event -> Maybe Action
keyEvent e = case (spy "key: " $ KE.key <$> KE.fromEvent e) of
  Just "a"     -> Just $ Mode AddMode
  Just "s"     -> Just $ Mode SelectMode
  Just "r"     -> Just $ Rotate
  Just "+"     -> Just $ ChangeTime true
  Just "-"     -> Just $ ChangeTime false
  Just "d"     -> Just $ Delete
  _            -> Nothing

wheelEvent :: E.Event -> Maybe Action
wheelEvent e = case (spy "key: " $ WE.deltaY <$> WE.fromEvent e) of
  Just 1.0    -> Just $ ChangeTime true
  Just (-1.0) -> Just $ ChangeTime false
  _            -> Nothing
 

mode :: Mode -> UI -> UI
--Go to add mode creates a dummy portal
mode AddMode ui = ui {partialPortal = Just {entry : {x: 0, y: 0}, exit : Nothing}, mode = AddMode}
mode SelectMode ui = ui {partialPortal = Nothing, mode = SelectMode}

---- click down validate the entry portal
--mouseDown :: Pos -> UI -> UI
--mouseDown pos ui@{partialPortal : Just {entry : entry, exit : Nothing}, mode : AddMode} = 
--              ui {partialPortal = Just {entry : entry, exit : Just {pos : pos, time : 0, dir : N}}}
--mouseDown _ ui = ui
--
--mouseMove :: Pos -> UI -> UI
---- Move the entry portal
--mouseMove pos ui@{partialPortal : Just {entry : entry, exit : Nothing}, mode : AddMode} = 
--              ui {partialPortal = Just {entry : pos, exit : Nothing}}
---- Move the exit portal
--mouseMove pos ui@{partialPortal : Just {entry : entry, exit : Just {pos : _, time : time, dir : dir}}, mode : AddMode} = 
--              ui {partialPortal = Just {entry : entry, exit : Just {pos : pos, time : time, dir : dir}}}
--mouseMove _ ui = ui
--
--mouseUp :: Pos -> UI -> UI
---- mouse up validates the portal
--mouseUp pos ui@{partialPortal : Just {entry : entry, exit : Just exit}, mode : AddMode} = 
--            ui {partialPortal = Nothing, initUniv {portals = {entry : entry, exit : exit} : ui.initUniv.portals }}
--mouseUp _ ui = ui

rotate :: UI -> UI
rotate ui@{partialPortal : Just {entry : entry, exit : Just {pos : pos, time : time, dir : dir}}, mode : AddMode} = 
       ui {partialPortal = Just {entry : entry, exit : Just {pos : pos, time : time, dir : turnRel Right_ dir}}}
rotate ui = ui

changeTime :: Boolean -> UI -> UI
changeTime p ui@{partialPortal : Just {entry : entry, exit : Just {pos : pos, time : time, dir : dir}}, mode : AddMode} = 
             ui {partialPortal = Just {entry : entry, exit : Just {pos : pos, time : if p then time + 1 else time - 1, dir : dir}}}
changeTime _ ui = ui

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter


updateUI :: (PTD -> PTD) -> UI -> UI
updateUI f ui = case ui.selItem of
  Just sel -> trace "updating" \_ -> updateUI' sel f ui
  Nothing -> ui 

updateUI' :: SelItem -> (PTD -> PTD) -> UI -> UI
updateUI' {itemType: EntryPortal, itemIndex: i} f = over (_initUniv <<< _portals <<< ix i <<< _entry) (toPos f)
updateUI' {itemType: ExitPortal, itemIndex: i} f  = over (_initUniv <<< _portals <<< ix i <<< _exit) f
updateUI' {itemType: Entry, itemIndex: i} f       = over (_initUniv <<< _consumers <<< ix i) (toPos f) 
updateUI' {itemType: Exit, itemIndex: i} f        = over (_initUniv <<< _emitters <<< ix i) f 
updateUI' _ _ = undefined

toPos :: (PTD -> PTD) -> Pos -> Pos 
toPos f p = _.pos $ f ({pos: p, time: 0, dir: N})


getPartialPortal :: PartialPortal -> Maybe Portal
getPartialPortal {entry : _, exit : Nothing} = Nothing
getPartialPortal {entry : entry, exit : Just exit} = Just {entry, exit}


