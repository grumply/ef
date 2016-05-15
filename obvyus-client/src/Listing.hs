{-# language RecordWildCards #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
{-# language NoMonomorphismRestriction #-}
module Listing where

import Ef
import Ef.Event

import Silicon

import qualified GHCJS.DOM.Types as T

import Unsafe.Coerce


data Listing k
    = Listing
          { _upvoteClicks :: forall self super. (Signal self super T.MouseEvent,k)
          , _downvoteClicks :: forall self super. (Signal self super T.MouseEvent,k)

          , _voteCount :: (Int,k)
          , _setVoteCount :: Int -> k
          , _voteCountNode :: (Node,k)
          , _setVoteCountNode :: Node -> k
          , _incrementVoteCount :: k
          , _decrementVoteCount :: k

          , _titleText :: (String,k)
          , _setTitleText :: String -> k
          , _titleTextNode :: (Node,k)
          , _setTitleTextNode :: Node -> k

          , _commentCount :: (Int,k)
          , _setCommentCount :: Int -> k
          , _commentCountNode :: (Node,k)
          , _setCommentCountNode :: Node -> k
          , _incrementCommentCount :: k
          , _decrementCommentCount :: k

          , _linkDomainText :: (String,k)
          , _setLinkDomainText :: String -> k
          , _linkDomainTextNode :: (Node,k)
          , _setLinkDomainTextNode :: Node -> k
          }
    | forall self super. UpvoteClicks (Signal self super T.MouseEvent -> k)
    | forall self super. DownvoteClicks (Signal self super T.MouseEvent -> k)

    | VoteCount (Int -> k)
    | SetVoteCount Int k
    | VoteCountNode (Node -> k)
    | SetVoteCountNode Node k
    | IncrementVoteCount k
    | DecrementVoteCount k

    | TitleText (String -> k)
    | SetTitleText String k
    | TitleTextNode (Node -> k)
    | SetTitleTextNode Node k

    | CommentCount (Int -> k)
    | SetCommentCount Int k
    | CommentCountNode (Node -> k)
    | SetCommentCountNode Node k
    | IncrementCommentCount k
    | DecrementCommentCount k

    | LinkDomainText (String -> k)
    | SetLinkDomainText String k
    | LinkDomainTextNode (Node -> k)
    | SetLinkDomainTextNode Node k

instance Ma Listing Listing where
  ma use Listing{..} (UpvoteClicks sk) = use (snd _upvoteClicks) (sk $ fst _upvoteClicks)

upvoteClicks :: (Monad super, '[Listing] :> self)
             => Narrative self (Narrative self' super) (Signal self' super T.MouseEvent)
upvoteClicks = self (UpvoteClicks id)

downvoteClicks :: (Monad super, '[Listing] :> self)
               => Narrative self (Narrative self' super) (Signal self' super T.MouseEvent)
downvoteClicks = self (DownvoteClicks id)

voteCount = self (VoteCount id)
setVoteCount c = self (SetVoteCount c ())
voteCountNode = self (VoteCountNode id)
setVoteCountNode n = self (SetVoteCountNode n ())
incrementVoteCount = self (IncrementVoteCount ())
decrementVoteCount = self (DecrementVoteCount ())

titleText = self (TitleText id)
setTitleText t = self (SetTitleText t ())
titleTextNode = self (TitleTextNode id)
setTitleTextNode n = self (SetTitleTextNode n ())

commentCount = self (CommentCount id)
setCommentCount c = self (SetCommentCount c ())
commentCountNode = self (CommentCountNode id)
setCommentCountNode n = self (SetCommentCountNode n ())
incrementCommentCount = self (IncrementCommentCount ())
decrementCommentCount = self (DecrementCommentCount ())

linkDomainText = self (LinkDomainText id)
setLinkDomainText t = self (SetLinkDomainText t ())
linkDomainTextNode = self (LinkDomainTextNode id)
setLinkDomainTextNode n = self (SetLinkDomainTextNode n ())
