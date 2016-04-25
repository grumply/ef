module Modal.Notification where

import Ef

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

import Control.Monad
import Data.Maybe

import Prelude hiding (span)

-- Partial implementation of a Toastr-based equivalent.

-- noteTitleStyle = do
--   fontWeight =: CSS.bold

-- noteMessageStyle = do
--   wordWrap =: breakWord

data Notes k
    = Notes
          { noteCount :: (Int,k)
          , noteCountSetter :: Int -> k

          , queuedNoteNodes :: ([NoteType],k)
          , queuedNoteNodesSetter :: [NoteType] -> k

          , notesEnabled :: (Bool,k)
          , notesEnabledSetter :: Bool -> k

          , activeNotes :: (Int,k)
          , activeNotesSetter :: Int -> k

          , activeNoteNodes :: ([Node],k)
          , activeNoteNodesSetter :: [Node] -> k

          , maxActiveNotes :: (Int,k)
          , maxActiveNotesSetter :: Int -> k
          }
    | GetNoteCount (Int -> k)
    | SetNoteCount Int k

    | GetQueuedNoteNodes ([NoteType] -> k)
    | SetQueuedNoteNodes [NoteType] k

    | GetNotesEnabled (Bool -> k)
    | SetNotesEnabled Bool k

    | GetActiveNotes (Int -> k)
    | SetActiveNotes Int k

    | GetActiveNoteNodes ([Node] -> k)
    | SetActiveNoteNodes [Node] k

    | GetMaxActiveNotes (Int -> k)
    | SetMaxActiveNotes Int k

getNoteCount = self (GetNoteCount id)
setNoteCount nc = self (SetNoteCount nc ())

getQueuedNoteNodes = self (GetQueuedNoteNodes id)
setQueuedNoteNodes ns = self (SetQueuedNoteNodes ns ())

getNotesEnabled = self (GetNotesEnabled id)
setNotesEnabled b = self (SetNotesEnabled b ())

getActiveNotes = self (GetActiveNotes id)
setActiveNotes an = self (SetActiveNotes an ())

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

  ma use Notes{..} (GetActiveNotes ik)       = use (snd activeNotes) (ik $ fst activeNotes)
  ma use Notes{..} (SetActiveNotes ns k)     = use (activeNotesSetter ns) k

  ma use Notes{..} (GetActiveNoteNodes nsk)  = use (snd activeNoteNodes) (nsk $ fst activeNoteNodes)
  ma use Notes{..} (SetActiveNoteNodes ns k) = use (activeNoteNodesSetter ns) k

  ma use Notes{..} (GetMaxActiveNotes ik)    = use (snd maxActiveNotes) (ik $ fst maxActiveNotes)
  ma use Notes{..} (SetMaxActiveNotes i k)   = use (maxActiveNotesSetter i) k

-- set max active notes based on device width and height
-- add text to notetype
-- add duration to notetype
-- add slide-off-page ability for note removal
-- push notes apart so they don't stack
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
      in return $ fs .= ns { queuedNoteNodes = (qnn,qnnGetter) }

  , notesEnabled = (True,return)
  , notesEnabledSetter = \ne fs ->
      let ns = view fs
          (_,neGetter) = notesEnabled ns
      in return $ fs .= ns { notesEnabled = (ne,neGetter) }

  , activeNotes = (0,return)
  , activeNotesSetter = \an fs ->
      let ns = view fs
          (_,anGetter) = activeNotes ns
      in return $ fs .= ns { activeNotes = (an,anGetter) }

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

-- embed a note and create a delayed removal of it
newNote noteType = do
  ne <- getNotesEnabled
  when ne $ void $ do
    man <- getMaxActiveNotes
    an <- getActiveNotes
    with fusion $
      if an < man
        then do
          i <- super newNoteId
          (n,(name,stopClickListen)) <- embedWith prepend $ note i noteType
          super $ do
            sig <- construct ()
            incrementActiveNotes
            addActiveNote n
            behavior' sig $ \_ _ ->
              with fusion $ void $ do
                x <- extract name
                liftIO $ print (isJust x)
                super $ do
                  stopClickListen
                  decrementActiveNotes
                  mn <- extractQueuedNote
                  forM_ mn newNote
            bufferDelay 8000000 () sig
        else super $ queueNote noteType

queueNote noteType = do
  qnn <- getQueuedNoteNodes
  setQueuedNoteNodes (qnn ++ [noteType])

addActiveNote n = do
  ann <- getActiveNoteNodes
  setActiveNoteNodes (ann ++ [n])

newNoteId = do
  i <- getNoteCount
  setNoteCount $! i + 1
  return i

decrementActiveNotes = do
  an <- getActiveNotes
  setActiveNotes $! an - 1

incrementActiveNotes = do
  an <- getActiveNotes
  setActiveNotes $! an + 1

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

noteTopRightStyle = do
  top   =: px 12
  right =: px 12

data NoteType = SuccessNote | InfoNote | WarningNote | ErrorNote

note i noteType = Named {..}
  where

    name = "note" ++ show i

    tag = division

    styles = do
      position =: fixed
      zIndex =: int 999999
      pointerEvents =: none
      top =: px 12
      right =: px 12

    element = do
      stopClickListen <- embed $ noteContent name noteType
      return (name,stopClickListen)

okLink noteName = Atom {..}
  where

    tag = anchor

    styles = do
      CSS.content =: quoted <| string "\\ue207"
      fontFamily  =: string "Glyphicons Regular"
      cursor      =: pointer
      marginRight =: px (-4)

    element = do
      -- _ <- embed okGlyph
      setAttr "href" "#close"
      (clicks,stopClickListen) <- listen E.click id interceptOpts
      behavior clicks $ \_ _ ->
       with fusion $ void $ do
         _ <- extract noteName
         super stopClickListen
      return stopClickListen

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

noteContent noteName noteType = Atom {..}
  where

    tag = division

    styles = do
      position =: relative
      pointerEvents =: auto
      overflow =: hidden
      margin =: px3 0 0 6
      padding =: px4 15 15 15 50
      borderRadius =: px 3
      backgroundRepeat =: noRepeat
      boxShadow =: spaces <| str zero zero (px 12) (hex 0x999)
      color =: hex 0xfff
      opacity =: dec 0.8
      CSS.filter =: alpha (eq opacity (int 80))
      backgroundColor =: (hex $
        case noteType of
          SuccessNote -> 0x51a351
          InfoNote    -> 0x2f96b4
          WarningNote -> 0xf89406
          ErrorNote   -> 0xbd362f)

    element = do
      responsive width (ems 11) (ems 18) (ems 25) (px 300)
      stopClickListen <- embed $ okLink noteName
      super $ globalHoverStyle (individual noteName) $ do
        boxShadow =: spaces <| str zero zero (px 12) white
        opacity =: one
        CSS.filter =: alpha (eq opacity (int 100))
        cursor =: pointer
      return stopClickListen


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
