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
-- Partial implementation of a Toastr-based equivalent.

-- noteTitleStyle = do
--   fontWeight =: CSS.bold

-- noteMessageStyle = do
--   wordWrap =: breakWord

data Notes k
    = Notes
          { noteCount :: (Int,k)
          , noteCountSetter :: Int -> k

          , queuedNoteNodes :: forall self super. ([NoteType self super ()],k)
          , queuedNoteNodesSetter :: forall self super. [NoteType self super ()] -> k

          , notesEnabled :: (Bool,k)
          , notesEnabledSetter :: Bool -> k

          , activeNoteCount :: (Int,k)
          , activeNoteCountSetter :: Int -> k

          , activeNoteNodes :: ([(Int,Node)],k)
          , activeNoteNodesSetter :: [(Int,Node)] -> k

          , maxActiveNotes :: (Int,k)
          , maxActiveNotesSetter :: Int -> k
          }
    | GetNoteCount (Int -> k)
    | SetNoteCount Int k

    | forall self super. GetQueuedNoteNodes ([NoteType self super ()] -> k)
    | forall self super. SetQueuedNoteNodes [NoteType self super ()] k

    | GetNotesEnabled (Bool -> k)
    | SetNotesEnabled Bool k

    | GetActiveNoteCount (Int -> k)
    | SetActiveNoteCount Int k

    | GetActiveNoteNodes ([(Int,Node)] -> k)
    | SetActiveNoteNodes [(Int,Node)] k

    | GetMaxActiveNotes (Int -> k)
    | SetMaxActiveNotes Int k

getNoteCount = self (GetNoteCount id)
setNoteCount nc = self (SetNoteCount nc ())

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

instance Ma Notes Notes where
  ma use Notes{..} (GetNoteCount nck)        = use (snd noteCount) (nck $ fst noteCount)
  ma use Notes{..} (SetNoteCount nc k)       = use (noteCountSetter nc) k

  ma use Notes{..} (GetQueuedNoteNodes nsk)  = use (snd queuedNoteNodes) (nsk $ fst queuedNoteNodes)
  ma use Notes{..} (SetQueuedNoteNodes ns k) = use (queuedNoteNodesSetter ns) k

  ma use Notes{..} (GetNotesEnabled bk)      = use (snd notesEnabled) (bk $ fst notesEnabled)
  ma use Notes{..} (SetNotesEnabled b k)     = use (notesEnabledSetter b) k

  ma use Notes{..} (GetActiveNoteCount ik)   = use (snd activeNoteCount) (ik $ fst activeNoteCount)
  ma use Notes{..} (SetActiveNoteCount ns k) = use (activeNoteCountSetter ns) k

  ma use Notes{..} (GetActiveNoteNodes nsk)  = use (snd activeNoteNodes) (nsk $ fst activeNoteNodes)
  ma use Notes{..} (SetActiveNoteNodes ns k) = use (activeNoteNodesSetter ns) k

  ma use Notes{..} (GetMaxActiveNotes ik)    = use (snd maxActiveNotes) (ik $ fst maxActiveNotes)
  ma use Notes{..} (SetMaxActiveNotes i k)   = use (maxActiveNotesSetter i) k


-- X set max active notes based on device width and height
-- add text to notetype
-- add duration to notetype
-- add slide-off-page ability for note removal
-- - push notes apart so they don't stack
-- get close button working
notes = Notes
  { noteCount = (0,return)
  , noteCountSetter = \nc fs ->
      let ns = view fs
          (_,ncGetter) = noteCount ns
      in return $ fs .= ns { noteCount = (nc,ncGetter) }

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

-- embed a note and create a delayed removal of it
newNote :: (Monad super, Lift IO super, Web <: self, '[Notes] <: self)
        => NoteType self super res -> Narrative self super (Promise (Maybe (Promise res)))
newNote noteType = delayed 0 $ do
  ne <- getNotesEnabled
  if ne
    then do
      man <- getMaxActiveNotes
      an <- getActiveNoteCount
      with fusion $
        if an < man
          then do
            i <- super newNoteId
            p <- newPromise
            ans <- super getActiveNoteNodes
            offset <- super $ calculateNoteOffset ans
            liftIO $ print offset
            embedResult <- embedWith prepend $ note i offset noteType
            let (n,(name,(res,(closeClicks,stopClickListen)))) = embedResult
            (_,styled) <- super $ using n $ style $ return ()
            fulfill p res
            super $ do
              incrementActiveNoteCount
              addActiveNote i n
              behavior' closeClicks $ \_ _ -> remover i name stopClickListen
              delayed (1000000 * noteDurationSeconds noteType)$ do
                remover i name stopClickListen
                clearSignal closeClicks
            return $ Just p
          else do
            p <- newPromise
            let modifiedNoteInnerContent =
                  (noteInnerContent noteType) {
                    element = do
                      x <- element $ noteInnerContent noteType
                      fulfill p x
                      return ()
                    }
                modifiedNoteType = noteType {
                  noteInnerContent = modifiedNoteInnerContent
                  }
            super $ queueNote modifiedNoteType
            return $ Just p
    else
      return Nothing
  where
    remover i name stopClickListen = void $ do
      with name $ style $ opacity =: zero
      delayed 1000000 $ with fusion $ do
        delete name
        super $ removeActiveNoteWithId i
        super recalculateNoteOffsets
        super $ do
          stopClickListen
          decrementActiveNoteCount
          mn <- extractQueuedNote
          -- have to make the recursive call non-recursive to avoid nested calls to 'with fusion'
          -- this will just delay the call until the next iteration of the main event loop
          delayed 0 (forM_ mn newNote)

removeActiveNoteWithId i = do
  ans <- getActiveNoteNodes
  let newans = Prelude.filter (\(n,note) -> n /= i) ans
  setActiveNoteNodes newans

recalculateNoteOffsets = do
  ans <- getActiveNoteNodes
  _ <- foldM (\acc (_,n) -> do
               using n $ style $ top =: px (round acc)
               return (acc + 64 + 12)
             ) 12 ans
  return ()

calculateNoteOffset = fmap round . go 12
  where
    go acc [] = return acc
    go acc (x@(_,n):ns) = go (acc + 64 + 12) ns

delayedWith s t f = do
  (sig,_) <- mapSignal' (const ()) s
  behavior' sig $ \Reactor{..} _ -> f >> end
  bufferDelay t () sig
  clearSignal sig

delayed t f = do
  p <- newPromise
  sig <- construct ()
  behavior' sig $ \Reactor{..} _ -> f >>= fulfill p >> end
  bufferDelay t () sig
  return p

queueNote noteType = do
  qnn <- getQueuedNoteNodes
  setQueuedNoteNodes (qnn ++ [noteType])

addActiveNote i n = do
  ann <- getActiveNoteNodes
  setActiveNoteNodes (ann ++ [(i,n)])

newNoteId = do
  i <- getNoteCount
  setNoteCount $! i + 1
  return i

decrementActiveNoteCount = do
  an <- getActiveNoteCount
  setActiveNoteCount $! an - 1

incrementActiveNoteCount = do
  an <- getActiveNoteCount
  setActiveNoteCount $! an + 1

extractQueuedNote = do
  qnn <- getQueuedNoteNodes
  case qnn of
    [] -> return Nothing
    (x:xs) -> do
      setQueuedNoteNodes xs
      return (Just x)

noteCloseButtonStyle = do
  position       =: relative
  right          =: ems (-0.3)
  top            =: ems (-0.3)
  float          =: right
  fontSize       =: px 20
  fontWeight     =: CSS.bold
  color          =: hex 0xfff
  textShadow     =: spaces <| str zero (px 1) zero (hex 0xfff)
  opacity        =: dec 0.8
  CSS.filter     =: alpha (eq opacity (int 80))

noteCloseButtonHoverStyle = globalHoverStyle (classified "noteClose") $ do
  color          =: white
  textDecoration =: none
  cursor         =: pointer
  opacity        =: dec 0.4
  CSS.filter     =: alpha (eq opacity (int 40))

noteCloseButtonFocusStyle = globalFocusStyle (classified "noteClose") $ do
  color          =: white
  textDecoration =: none
  cursor         =: pointer
  opacity        =: dec 0.4
  CSS.filter     =: alpha (eq opacity (int 40))

data NoteType self super result
  = SuccessNote
    { noteDurationSeconds :: Int
    , noteInnerContent :: Atom self super result
    }
  | InfoNote
    { noteDurationSeconds :: Int
    , noteInnerContent :: Atom self super result
    }
  | WarningNote
    { noteDurationSeconds :: Int
    , noteInnerContent :: Atom self super result
    }
  | ErrorNote
    { noteDurationSeconds :: Int
    , noteInnerContent :: Atom self super result
    }

note i offTop noteType = Named {..}
  where

    name = "note" ++ show i

    tag = division

    styles = do
      position      =: fixed
      height        =: px 64
      padding       =: px4 8 8 8 40
      overflow      =: hidden
      zIndex        =: int 999999
      pointerEvents =: none
      top           =: px offTop
      right         =: px 12
      opacity       =: dec 0.8
      transitions $ spaces <| str opacity (sec 1) easeIn
      transitions $ spaces <| str top (sec 0.8) (cubicBezier(0.02, 0.01, 0.47, 1))

    element = do
      closeInterface <- embed $ noteContent name noteType
      return (name,closeInterface)

okLink noteName = Atom {..}
  where

    tag = anchor

    styles = do
      float       =: left
      CSS.content =: quoted <| string "\\ue207"
      fontFamily  =: string "Glyphicons Regular"
      cursor      =: pointer
      marginRight =: px (-4)

    element = do
      -- _ <- embed okGlyph
      setAttr "href" "#close"
      (clicks,stopClickListen) <- listen E.click id interceptOpts
      return (clicks,stopClickListen)

okGlyph = Atom {..}
  where

    tag = span

    styles = do
      width =: px 50
      top            =: px 8
      position       =: relative
      textDecoration =: none
      fontSize       =: px 25
      color          =: darkcyan

    element =
      glyph gOk

noteContent :: (Monad super, Lift IO super, Web <: self)
            => String -> NoteType self super res -> Atom self super (res,(Signal self super T.MouseEvent,Narrative self super ()))
noteContent noteName noteType = Atom {..}
  where

    tag = division

    styles = do
      width            =: px 300
      position         =: relative
      height           =: px 64
      pointerEvents    =: auto
      overflow         =: hidden
      borderRadius     =: px 3
      backgroundRepeat =: noRepeat
      boxShadow        =: spaces <| str zero zero (px 12) (hex 0x999)
      color            =: hex 0xfff
      opacity          =: dec 1
      CSS.filter       =: alpha (eq opacity (int 80))
      backgroundColor  =: hex
        (case noteType of
          SuccessNote{} -> 0x51a351
          InfoNote{}    -> 0x2f96b4
          WarningNote{} -> 0xf89406
          ErrorNote{}   -> 0xbd362f)

    element = do
      responsive width (ems 11) (ems 18) (ems 25) (px 300)
      res <- embed (noteInnerContent noteType)
      closeInterface <- embed $ okLink noteName
      super $ globalHoverStyle (individual noteName) $ do
        boxShadow  =: spaces <| str zero zero (px 12) white
        opacity    =: one
        CSS.filter =: alpha (eq opacity (int 100))
        cursor     =: pointer
      return (res,closeInterface)
      


{-
/*Additional properties for button version
 iOS requires the button element instead of an anchor tag.
 If you want the anchor version, it requires `href="#"`.*/
button.toast-close-button {
  padding: 0;
  cursor: pointer;
  background: transparent;
  border: 0;
  -webkit-appearance: none;
}
-}
