{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Mop.IO where

import Mop.Core

import Control.Monad

import qualified System.IO as IO
import qualified Foreign.Ptr as IO

--------------------------------------------------------------------------------
-- Standard File Interfaces

type FileRead = '[ReadingFile]

type FileWrite = '[WritingFile]

type FileAppend = '[AppendingFile]

type SimpleFile     = FileRead ∪ FileWrite ∪ FileAppend

type File           = FileInit           ∪ FileGet       ∪ FilePut       ∪ HandleControl

type BinaryFile     = BinaryFileInit     ∪ BinaryFileGet ∪ BinaryFilePut ∪ HandleControl

type TempFile       = TempFileInit       ∪ FileGet       ∪ FilePut       ∪ HandleControl

type TempBinaryFile = BinaryTempFileInit ∪ BinaryFileGet ∪ BinaryFilePut ∪ HandleControl

--------------------------------------------------------------------------------
-- File Components

type FileInit = '[OpeningFile,ClosingFile]
type BinaryFileInit = '[OpeningBinaryFile,ClosingFile]
type TempFileInit =
  '[OpeningFileTempFile,OpeningFileTempFileWithDefaultPermissions,ClosingFile]
type BinaryTempFileInit =
  '[OpeningBinaryFileTempFile,OpeningBinaryFileTempFileWithDefaultPermissions,ClosingFile]

type FileControl = '[GettingFileSize,SettingFileSize]

type FilePut = '[PutChar,PutString,PutStringLine,Print]
type BinaryFilePut = '[PutBuffer,PutBufferNonBlocking]

type FileGet = '[GetChar,GetLine,GetContents]
type BinaryFileGet = '[GetBuffer,GetBufferSome,GetBufferNonBlocking]

type HandleControl =
  '[Flushing,LookingAhead,CheckingEOF,Waiting,SettingNewlineMode
   ,Seeking,Telling
   ,SettingEcho,GettingEcho
   ,CheckingOpen,CheckingClosed
   ,CheckingReadable,CheckingWritable,CheckingSeekingable
   ,CheckingTerminalDevice,CheckingReady
   ,SettingBinaryMode
   ,SettingBuffering,GettingBuffering
   ,SettingPosn,GettingPosn
   ,ShowingHandle
   ]

--------------------------------------------------------------------------------
--

data OpeningFile k = OpeningFile IO.FilePath IO.IOMode (IO IO.Handle -> k)
-- open :: (Checking OpeningFile fs m,Lift IO m) => IO.FilePath -> IO.IOMode -> Pattern fs m
open :: (Lift IO m, Is OpeningFile symbols m)
     => FilePath -> IO.IOMode -> Pattern symbols m IO.Handle
open fp iom = join $ self (OpeningFile fp iom lift)

data Openable k = Openable (IO.FilePath -> IO.IOMode -> (IO IO.Handle,k))
opener :: Uses Openable fs m => Attribute Openable fs m
opener = Openable $ \fp iom -> (IO.openFile fp iom,return)

data ClosingFile k = ClosingFile IO.Handle k
data SettingBuffering k = SettingBuffering IO.Handle IO.BufferMode k
data GettingBuffering k = GettingBuffering IO.Handle (IO.BufferMode -> k)
data SettingEncoding k = SettingEncoding IO.Handle IO.TextEncoding k
data GettingEncoding k = GettingEncoding IO.Handle (Maybe IO.TextEncoding -> k)
data SettingNewlineMode k = SettingNewlineMode IO.Handle IO.NewlineMode k
data Flushing k = Flushing IO.Handle k
data GettingPosn k = GettingPosn IO.Handle (IO.HandlePosn -> k)
data SettingPosn k = SettingPosn IO.HandlePosn k
data Seeking k = Seeking IO.Handle IO.SeekMode Integer k
data Telling k = Telling IO.Handle (Integer -> k)
data CheckingOpen k = CheckingOpen IO.Handle (Bool -> k)
data CheckingClosed k = CheckingClosed IO.Handle (Bool -> k)
data CheckingReadable k = CheckingReadable IO.Handle (Bool -> k)
data CheckingWritable k = CheckingWritable IO.Handle (Bool -> k)
data CheckingSeekingable k = CheckingSeekingable IO.Handle (Bool -> k)
data CheckingTerminalDevice k = CheckingTerminalDevice IO.Handle (Bool -> k)
data SettingEcho k = SettingEcho IO.Handle Bool k
data GettingEcho k = GettingEcho IO.Handle (Bool -> k)
data ShowingHandle k = ShowingHandle IO.Handle (String -> k)
data Waiting k = Waiting IO.Handle Int (Bool -> k)
data CheckingReady k = CheckingReady IO.Handle (Bool -> k)
data GetChar k = GetChar IO.Handle (Char -> k)
data GetLine k = GetLine IO.Handle (String -> k)
data LookingAhead k = LookingAhead IO.Handle (Char -> k)
data GetContents k = GetContents IO.Handle (String -> k)
data PutChar k = PutChar IO.Handle Char k
data PutString k = PutString IO.Handle String k
data PutStringLine k = PutStringLine IO.Handle String k
data Print k = forall a. Show a => Print IO.Handle a k
data OpeningBinaryFile k = OpeningBinaryFile IO.FilePath IO.IOMode (IO.Handle -> k)
data SettingBinaryMode k = SettingBinaryMode IO.Handle Bool k
data PutBuffer k = forall a. PutBuffer IO.Handle (IO.Ptr a) Int k
data GetBuffer k = forall a. GetBuffer IO.Handle (IO.Ptr a) Int (Int -> k)
data GetBufferSome k = forall a. GetBufferSome IO.Handle (IO.Ptr a) Int (Int -> k)
data PutBufferNonBlocking k = forall a. PutBufferNonBlocking IO.Handle (IO.Ptr a) Int (Int -> k)
data GetBufferNonBlocking k = forall a. GetBufferNonBlocking IO.Handle (IO.Ptr a) Int (Int -> k)
data OpeningFileTempFile k = OpeningFileTempFile IO.FilePath String ((IO.FilePath,IO.Handle) -> k)
data OpeningBinaryFileTempFile k = OpeningBinaryFileTempFile IO.FilePath String ((IO.FilePath,IO.Handle) -> k)
data OpeningFileTempFileWithDefaultPermissions k = OpeningFileTempFileWithDefaultPermissions IO.FilePath String ((IO.FilePath,IO.Handle) -> k)
data OpeningBinaryFileTempFileWithDefaultPermissions k = OpeningBinaryFileTempFileWithDefaultPermissions IO.FilePath String ((IO.FilePath,IO.Handle) -> k)
data ReadingFile k = ReadingFile IO.FilePath (String -> k)
data WritingFile k = WritingFile IO.FilePath String k
data AppendingFile k = AppendingFile IO.FilePath String k
data GettingFileSize k = GettingFileSize IO.Handle (Integer -> k)
data SettingFileSize k = SettingFileSize IO.Handle Integer k
data CheckingEOF k = CheckingEOF IO.Handle (Bool -> k)
