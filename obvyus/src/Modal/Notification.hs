{-# language RankNTypes #-}
module Modal.Notification where

import Ef
import Data.Promise

import Carbon as CSS
import Helium
import Hydrogen
import Iron
import Neon

import Silicon

import Flex

import Modal
import Utility

import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Screen as S
import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.Types as T


import Control.Monad
import Data.Maybe

import Prelude hiding (span)

import Unsafe.Coerce

data Note self super result
  = Note
    { noteDurationSeconds :: Int
    , noteInnerContent :: Atom self super result
    }

data Notes k
    = Notes
          { -- uniqueness reservoir
            noteCount :: (Int,k)

            -- Notes queued for display
          , queuedNoteNodes :: forall self super. ([Note self super ()],k)
          , queuedNoteNodesSetter :: forall self super. [Note self super ()] -> k

            -- Whether notes are enabled for the application
          , notesEnabled :: (Bool,k)
          , notesEnabledSetter :: Bool -> k

            -- Number of notes currently visible
          , activeNoteCount :: (Int,k)
          , activeNoteCountSetter :: Int -> k

            -- Currently active notes
          , activeNoteNodes :: ([(Int,Node)],k)
          , activeNoteNodesSetter :: [(Int,Node)] -> k

            -- Maximum number of visible notes
          , maxActiveNotes :: (Int,k)
          , maxActiveNotesSetter :: Int -> k

            -- Container node for notes
          , noteContainerNode :: (Node,k)
          , noteContainerNodeSetter :: Node -> k
          }

    | GetFreshNoteId (Int -> k)

    | forall self super. GetQueuedNoteNodes ([Note self super ()] -> k)
    | forall self super. SetQueuedNoteNodes [Note self super ()] k

    | GetNotesEnabled (Bool -> k)
    | SetNotesEnabled Bool k

    | GetActiveNoteCount (Int -> k)
    | SetActiveNoteCount Int k

    | GetActiveNoteNodes ([(Int,Node)] -> k)
    | SetActiveNoteNodes [(Int,Node)] k

    | GetMaxActiveNotes (Int -> k)
    | SetMaxActiveNotes Int k

    | GetNoteContainerNode (Node -> k)
    | SetNoteContainerNode Node k

getFreshNoteId = self (GetFreshNoteId id)

getQueuedNoteNodes = self (GetQueuedNoteNodes id)
setQueuedNoteNodes ns = self (SetQueuedNoteNodes ns ())

getNotesEnabled = self (GetNotesEnabled id)
setNotesEnabled b = self (SetNotesEnabled b ())

getActiveNoteCount = self (GetActiveNoteCount id)
setActiveNoteCount an = self (SetActiveNoteCount an ())

getActiveNoteNodes = self (GetActiveNoteNodes id)
setActiveNoteNodes ns = self (SetActiveNoteNodes ns ())

getMaxActiveNotes = self (GetMaxActiveNotes id)
setMaxActiveNotes i = self (SetMaxActiveNotes i ())

getNoteContainerNode = self (GetNoteContainerNode id)
setNoteContainerNode n = self (SetNoteContainerNode n ())

instance Ma Notes Notes where
  ma use Notes{..} (GetFreshNoteId nck)       = use (snd noteCount) (nck $ fst noteCount)

  ma use Notes{..} (GetQueuedNoteNodes nsk)   = use (snd queuedNoteNodes) (nsk $ fst queuedNoteNodes)
  ma use Notes{..} (SetQueuedNoteNodes ns k)  = use (queuedNoteNodesSetter ns) k

  ma use Notes{..} (GetNotesEnabled bk)       = use (snd notesEnabled) (bk $ fst notesEnabled)
  ma use Notes{..} (SetNotesEnabled b k)      = use (notesEnabledSetter b) k

  ma use Notes{..} (GetActiveNoteCount ik)    = use (snd activeNoteCount) (ik $ fst activeNoteCount)
  ma use Notes{..} (SetActiveNoteCount ns k)  = use (activeNoteCountSetter ns) k

  ma use Notes{..} (GetActiveNoteNodes nsk)   = use (snd activeNoteNodes) (nsk $ fst activeNoteNodes)
  ma use Notes{..} (SetActiveNoteNodes ns k)  = use (activeNoteNodesSetter ns) k

  ma use Notes{..} (GetMaxActiveNotes ik)     = use (snd maxActiveNotes) (ik $ fst maxActiveNotes)
  ma use Notes{..} (SetMaxActiveNotes i k)    = use (maxActiveNotesSetter i) k

  ma use Notes{..} (GetNoteContainerNode nk)  = use (snd noteContainerNode) (nk $ fst noteContainerNode)
  ma use Notes{..} (SetNoteContainerNode n k) = use (noteContainerNodeSetter n) k


-- valid notes use requires calling:
--   setMaxActiveNotes
-- and
--   setNoteContainerNode
notes = Notes
  { noteCount =
      let start = 0
          update = \fs ->
            let ns = view fs
                (nc,ncGetter) = noteCount ns
                !nc' = nc + 1
            in return $ fs .= ns { noteCount = (nc',ncGetter) }
      in (start,update)

  , queuedNoteNodes = ([],return)
  , queuedNoteNodesSetter = \qnn fs ->
      let ns = view fs
          (_,qnnGetter) = queuedNoteNodes ns
      in return $ fs .= ns { queuedNoteNodes = (unsafeCoerce qnn,qnnGetter) }

  , notesEnabled = (True,return)
  , notesEnabledSetter = \ne fs ->
      let ns = view fs
          (_,neGetter) = notesEnabled ns
      in return $ fs .= ns { notesEnabled = (ne,neGetter) }

  , activeNoteCount = (0,return)
  , activeNoteCountSetter = \an fs ->
      let ns = view fs
          (_,anGetter) = activeNoteCount ns
      in return $ fs .= ns { activeNoteCount = (an,anGetter) }

  , activeNoteNodes = ([],return)
  , activeNoteNodesSetter = \ann fs ->
      let ns = view fs
          (_,annGetter) = activeNoteNodes ns
      in return $ fs .= ns { activeNoteNodes = (ann,annGetter) }

  , maxActiveNotes = (1,return)
  , maxActiveNotesSetter = \man fs ->
      let ns = view fs
          (_,manGetter) = maxActiveNotes ns
      in return $ fs .= ns { maxActiveNotes = (man,manGetter) }

  , noteContainerNode = (undefined,return)
  , noteContainerNodeSetter = \ncn fs ->
      let ns = view fs
          (_,ncnGetter) = noteContainerNode ns
      in return $ fs .= ns { noteContainerNode = (ncn,ncnGetter) }
  }

configureMaxNotes = do
  (w,h) <- liftIO $ do
    win      <- getWindow
    Just scr <- W.getScreen win
    liftM2 (,) (S.getWidth scr ) (S.getHeight scr)
  setMaxActiveNotes $
    if | w <= 400  -> 1
       | h >= 1200 -> 4
       | h >= 900  -> 3
       | h >= 480  -> 2
       | otherwise -> 1

noteContinerId = "noteContainer"

createNoteContainer = do
  fromJust <$> with fusion $ embedWith child noteContainerAtom
  where
    noteContainerAtom = Atom{..}
      where

        tag = division

        styles = do
          visibility =: hidden
          position   =: fixed
          top        =: px 12
          right      =: px 12

        element = return ()

-- -- embed a note and create a delayed removal of it
-- newNote :: (Monad super, Lift IO super, Web <: self, '[Notes] <: self)
--         => Note self super res -> Narrative self super (Promise (Maybe (Promise res)))
-- newNote noteType = delayed 0 $ do
--   ne <- getNotesEnabled
--   if ne
--     then do
--       man <- getMaxActiveNotes
--       an <- getActiveNoteCount
--       fmap join $ with fusion $
--         if an < man
--           then do
--             i <- super newNoteId
--             p <- newPromise
--             ans <- super getActiveNoteNodes
--             offset <- super $ calculateNoteOffset ans
--             liftIO $ print offset
--             embedResult <- embedWith prepend $ note i offset noteType
--             let (n,(name,(res,(closeClicks,stopClickListen)))) = embedResult
--             (_,styled) <- super $ using n $ style $ return ()
--             fulfill p res
--             super $ do
--               incrementActiveNoteCount
--               addActiveNote i n
--               behavior' closeClicks $ \_ _ -> remover i name stopClickListen
--               delayed (1000000 * noteDurationSeconds noteType)$ do
--                 remover i name stopClickListen
--                 clearSignal closeClicks
--             return $ Just p

--           else do
--             p <- newPromise
--             let modifiedNoteInnerContent =
--                   (noteInnerContent noteType) {
--                     element = do
--                       x <- element $ noteInnerContent noteType
--                       fulfill p x
--                       return ()
--                     }
--                 modifiedNoteType = noteType {
--                   noteInnerContent = modifiedNoteInnerContent
--                   }
--             super $ queueNote modifiedNoteType
--             return $ Just p

--     else
--       return Nothing

--   where
--     remover i name stopClickListen = void $ do
--       with name $ style $ opacity =: zero
--       delayed 1000000 $ with fusion $ do
--         delete name
--         super $ removeActiveNoteWithId i
--         super recalculateNoteOffsets
--         super $ do
--           stopClickListen
--           decrementActiveNoteCount
--           mn <- extractQueuedNote
--           -- have to make the recursive call non-recursive to avoid nested calls to 'with fusion'
--           -- this will just delay the call until the next iteration of the main event loop
--           delayed 0 (forM_ mn newNote)

-- removeActiveNoteWithId i = do
--   ans <- getActiveNoteNodes
--   let newans = Prelude.filter (\(n,note) -> n /= i) ans
--   setActiveNoteNodes newans

-- recalculateNoteOffsets = do
--   ans <- getActiveNoteNodes
--   _ <- foldM (\acc (_,n) -> do
--                using n $ style $ top =: px (round acc)
--                return (acc + 64 + 12)
--              ) 12 ans
--   return ()

-- calculateNoteOffset = fmap round . go 12
--   where
--     go acc [] = return acc
--     go acc (x@(_,n):ns) = go (acc + 64 + 12) ns

-- delayedWith s t f = do
--   (sig,_) <- mapSignal' (const ()) s
--   behavior' sig $ \Reactor{..} _ -> f >> end
--   bufferDelay t () sig
--   clearSignal sig

-- delayed t f = do
--   p <- newPromise
--   sig <- construct ()
--   behavior' sig $ \Reactor{..} _ -> f >>= fulfill p >> end
--   bufferDelay t () sig
--   return p

-- queueNote noteType = do
--   qnn <- getQueuedNoteNodes
--   setQueuedNoteNodes (qnn ++ [noteType])

-- addActiveNote i n = do
--   ann <- getActiveNoteNodes
--   setActiveNoteNodes (ann ++ [(i,n)])

-- newNoteId = do
--   i <- getNoteCount
--   setNoteCount $! i + 1
--   return i

-- decrementActiveNoteCount = do
--   an <- getActiveNoteCount
--   setActiveNoteCount $! an - 1

-- incrementActiveNoteCount = do
--   an <- getActiveNoteCount
--   setActiveNoteCount $! an + 1

-- extractQueuedNote = do
--   qnn <- getQueuedNoteNodes
--   case qnn of
--     [] -> return Nothing
--     (x:xs) -> do
--       setQueuedNoteNodes xs
--       return (Just x)

-- note i offTop noteType = Named {..}
--   where

--     name = "note" ++ show i

--     tag = division

--     styles = do
--       borderRadius  =: px 5
--       width         =: px 250
--       position      =: fixed
--       height        =: px 64
--       padding       =: px 8
--       zIndex        =: int 999999
--       pointerEvents =: none
--       top           =: px offTop
--       right         =: px 12
--       transitions $ spaces <| str top (sec 1) easeIn

--     element = do
--       closeInterface <- embed $ noteContent i name noteType
--       return (name,closeInterface)

-- noteContent :: (Monad super, Lift IO super, Web <: self)
--             => Int -> String -> Note self super res -> Atom self super (res,(Signal self super T.MouseEvent,Narrative self super ()))
-- noteContent i noteName noteType = Atom {..}
--   where

--     tag = division

--     styles = do
--       marginLeft       =: px 25
--       marginRight      =: px 20
--       height           =: px 64
--       pointerEvents    =: auto
--       overflow         =: hidden
--       backgroundRepeat =: noRepeat
--       boxShadow        =: spaces <| str zero zero (px 12) (hex 0x999)
--       color            =: hex 0xfff
--       opacity          =: zero
--       CSS.filter       =: alpha (eq opacity (int 80))
--       backgroundColor  =: hex 0xddd
--       helveticaNeue (weight 300) black (px 12)
--       transitions $ spaces <| str opacity (sec 0.8) easeIn

--     element = do
--       slf <- current
--       void $ style $ opacity =: one
--       _ <- embed obvyusIcon
--       res <- embed (noteInnerContent noteType)
--       closeInterface <- embed $ noteCloseButton i
--       super $ globalHoverStyle (individual noteName) $ do
--         boxShadow  =: spaces <| str zero zero (px 12) white
--         opacity    =: one
--         CSS.filter =: alpha (eq opacity (int 100))
--         cursor     =: pointer
--       return (res,closeInterface)

-- noteCloseButton i = Named {..}
--   where

--     name = "noteClose" ++ show i

--     tag = anchor

--     styles = do
--       position       =: relative
--       right          =: ems (-0.3)
--       top            =: ems (-0.3)
--       float          =: right
--       fontSize       =: px 20
--       fontWeight     =: CSS.bold
--       color          =: hex 0xfff
--       textShadow     =: spaces <| str zero (px 1) zero (hex 0xfff)
--       opacity        =: dec 0.8
--       CSS.filter     =: alpha (eq opacity (int 80))

--     element = do
--       super $ globalHoverStyle (classified name) $ do
--         color          =: white
--         textDecoration =: none
--         cursor         =: pointer
--         opacity        =: dec 0.4
--         CSS.filter     =: alpha (eq opacity (int 40))
--       super $ globalFocusStyle (classified name) $ do
--         color          =: white
--         textDecoration =: none
--         cursor         =: pointer
--         opacity        =: dec 0.4
--         CSS.filter     =: alpha (eq opacity (int 40))
--       setText "X"
--       setAttr "href" "#close"
--       listen E.click id listenOpts

-- obvyusIcon = Atom {..}
--   where

--     tag = span

--     styles = do
--       position       =: relative
--       textDecoration =: none
--       bgColor        =: darkcyan
--       left           =: px 8
--       top            =: px 8
--       verticalAlign  =: CSS.center
--       timesNewRoman CSS.bold white (px 15)

--     element = void $ setText "O"

