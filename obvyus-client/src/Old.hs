{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language NoMonomorphismRestriction #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language RecordWildCards #-}
{-# language RankNTypes #-}
{-# OPTIONS_GHC -O0 #-}
module Main where

import Ef
import Ef.Event
import Data.Promise
import Data.Queue
import Lotus
import Lotus.Event
import Lily

import Control.Monad

import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Node as N
import qualified GHCJS.DOM.Types as T

import Prelude hiding (log)

import Control.Monad
import Control.Concurrent

import Unsafe.Coerce

instance Lotus.Event.Event (Maybe (Int, Narrative '[HTML Web IO] (Narrative Web IO) ()))

div_ = child "div" Nothing

data Menu = Menu
    { interesting :: Node
    , provocative :: Node
    }

navbar :: (Lift IO super, Monad super, Web <: self)
       => Narrative '[HTML self super] (Narrative self super) Menu
navbar = do
    (_,menu) <- child "nav" (Just "navbar") $ do
        row
        (_,menu) <- child "a" (Just "hat") $ do
            col XS Nothing
            child "span" (Just "brand-letter") $ do
                setAttr "href" "#"
                addStyle "text-decoration" "none"
                addStyles
                    [ style "padding" "2px 5px"
                    , style "font-size" "15px"
                    , style "color" "white"
                    , style "background-color" "darkcyan"
                    , style "font-weight" "bold"
                    , style "font-family" "\"Times New Roman\",Serif"
                    ]
                setText "O"
            child "span" (Just "brand-name") $ do
                setAttr "href" "#"
                addStyle "text-decoration" "none"
                addStyles
                    [ style "padding-left" "12px"
                    , style "font-size" "13px"
                    , style "font-weight" "600"
                    , style "color" "black"
                    , style "font-family" "\"Helvetica Neue\",Helvetica,Arial"
                    ]
                setInner "Obvy<span style=\"font-weight:200;\
                                           \font-size:28px;\
                                           \top:4px;\
                                           \position:relative;\
                                           \color:rgba(0,0,0,0.5)\
                                           \\">|</span>us"
            child "span" Nothing $ addStyle "padding" "10px"
            let linkStyles =
                        [ style "text-decoration" "none"
                        , style "margin-right" "5px"
                        , style "font-size" "14px"
                        , style "color" "gray"
                        ]
            i <- ahref "Interesting" "/interesting" linkStyles
            p <- ahref "Provocative" "/provocative" linkStyles
            return $ Menu i p
        child "a" Nothing $ do
            end
            setText "Login"
            setAttr "href" "#/login"
            addStyles
                [ style "font-family" "\"Helvetica Neue\",Helvetica,Arial"
                , style "text-decoration" "none"
                , style "font-size" "12px"
                , style "font-weight" "600"
                , style "color" "gray"
                , style "margin-top" "15px"
                ]
        return menu
    return menu

divider = do
    child "div" Nothing $ do
        row
        child "div" Nothing (column 1)
        child "div" Nothing $ do
            column 10
            child "hr" Nothing $ do
                addStyles
                    [ style "border" "0"
                    , style "height" "1px"
                    , style "background-image"
                        "linear-gradient(to right,\
                          \ rgba(0, 0, 0, 0),\
                          \ rgba(0, 0, 0, 0.75),\
                          \ rgba(0, 0, 0, 0)\
                          \)"
                    ]
        child "div" Nothing (column 1)

ahref :: (Lift IO super, Monad super, Web <: self)
      => String
      -> String
      -> [(String,String)]
      -> Narrative '[HTML self super] (Narrative self super) Node
ahref txt lnk styles = do
    let hashPath = "#" ++ lnk
    (a,_) <- child "a" Nothing $ do
        setAttr "href" hashPath
        setText txt
        addStyles styles
    return a

mkActive :: (Lift IO super, Monad super, Web <: self)
         => Node -> Narrative '[HTML self super] (Narrative self super) ()
mkActive cur = do
    super $ log "Making active"
    super $ void $ with_ cur $ void $ replaceStyles [style "color" "black"]

footer :: (Lift IO super, Monad super, Web <: self)
       => Narrative '[HTML self super] (Narrative self super) ()
footer = void $ do
    divider
    child "div" Nothing $ do
        row
        child "div" Nothing $ do
            column 12
            center
            child "div" Nothing $ do
                child "a" Nothing $ do
                    addStyles
                        [ style "text-decoration" "none"
                        , style "color" "gray"
                        , style "font-size" "12px"
                        ]
                    setAttr "href" "#/about"
                    setText "About"
                child "span" Nothing $ do
                    addStyles
                        [ style "color" "gray"
                        , style "font-size" "12px"
                        , style "margin" "0px 10px"
                        ]
                    setText "|"
                child "a" Nothing $ do
                    addStyles
                        [ style "text-decoration" "none"
                        , style "color" "gray"
                        , style "font-size" "12px"
                        ]
                    setAttr "href" "#/privacy"
                    setText "Privacy"
        child "div" Nothing $ do
            column 12
            center
            child "span" Nothing $ do
                addStyles
                    [ style "color" "gray"
                    , style "font-size" "12px"
                    ]
                setText "© 2016 S. M. Hickman"

data Tag = Tag
    { tagValue :: String
    , tagColor :: String
    }

data Listing = Listing
    { listingTitle :: String
    , listingAuthor :: String
    , listingVotes :: Int
    , listingComments :: Int
    , listingTags :: [Tag]
    , listingShortcode :: Int
    , listingLink :: String
    }

data LiveListing self super = LiveListing
    { liveListing :: Listing
    , listingNode :: Node
    , voteCountSignal :: Signal self super (Maybe (Int,Narrative '[HTML self super] (Narrative self super) ()))
    , commentCountSignal :: Signal self super Int
    , voteSignal :: Signal self super T.MouseEvent
    -- upvote and downvote signals are an attempt to be able to force cleanup of the behaviors involved with handling votes
    , upvoteSignal :: Signal self super T.MouseEvent
    , downvoteSignal :: Signal self super T.MouseEvent
    }

listing :: (Lift IO super, Monad super, Web <: self)
        => Listing
        -> Narrative '[HTML self super] (Narrative self super) (LiveListing self super)
listing liveListing@Listing{..} = do
    (listingNode,((upA,upvoteSignal),(downA,downvoteSignal))) <- titleLine
    ((voteCount,voteCountSignal),commentCountSignal) <- byLine
    votes <- lift $ newMVar 0
    doc <- liftIO' getDocument
    super $ event $ \e -> do
        (behavior e) upvoteSignal $ \Reactor{..} _ -> do
            b <- liftIO' $ N.contains doc $ Just $ dom upA
            unless b die
            voteStatus <- lift $ takeMVar votes
            case voteStatus of
                (-1) -> do
                    lift $ putMVar votes 0
                    (trigger e) voteCountSignal $ Just (1,void $ replaceStyles [ style "color" "black"])
                0 -> do
                    lift $ putMVar votes 1
                    (trigger e) voteCountSignal $ Just (1,void $ replaceStyles [ style "color" "darkgreen"])
                1 -> lift $ putMVar votes 1
        (behavior e) downvoteSignal $ \Reactor{..} _ -> do
            b <- liftIO' $ N.contains doc $ Just $ dom downA
            unless b die
            voteStatus <- lift $ takeMVar votes
            case voteStatus of
                (-1) -> lift $ putMVar votes voteStatus
                0 -> do
                    lift $ putMVar votes (-1)
                    (trigger e) voteCountSignal $ Just (-1,void $ replaceStyles [ style "color" "darkred"])
                1 -> do
                    lift $ putMVar votes 0
                    (trigger e) voteCountSignal $ Just (-1,void $ replaceStyles [ style "color" "black"])
        (behavior e) voteCountSignal $ \Reactor{..} ev -> do
            case ev of
                Nothing -> die
                Just (updateVal,mods) ->
                    void $ with_ voteCount $ do
                        Just txt <- demand =<< getText
                        let cur = read txt
                        setText $ show $ cur + updateVal
                        mods
    return LiveListing {..}
    where

        titleLine = do
            (listingNode,arrows) <-
                child "div" Nothing $ do
                    row
                    arrowComponent <* listingLinkComponent
            return (listingNode,arrows)

        byLine = do
            (_,(voteCountSignal,commentCountSignal)) <-
                child "div" Nothing $ do
                    row
                    addStyles [ style "margin-bottom" "10px" ]
                    (,) <$> voteCountComponent <*> bylineComponent
            return (voteCountSignal,commentCountSignal)

        arrowComponent = do
            (_,arrows) <- child "div" Nothing $ do
                column 1
                (_,arrows) <- child "div" Nothing $ do
                    (upvoteArrow,(upvoteArrowClicks,_)) <- child "a" Nothing $ do
                        row
                        center
                        addStyles
                            [ style "width" "0"
                            , style "height" "0"
                            , style "border-left" "8px solid transparent"
                            , style "border-right" "8px solid transparent"
                            , style "border-bottom" "8px solid black"
                            , style "margin-bottom" "8px"
                            ]
                        listen E.click (Options False False)
                    (downvoteArrow,(downvoteArrowClicks,_)) <- child "a" Nothing $ do
                        row
                        center
                        addStyles
                            [ style "width" "0"
                            , style "height" "0"
                            , style "border-left" "8px solid transparent"
                            , style "border-right" "8px solid transparent"
                            , style "border-top" "8px solid black"
                            ]
                        listen E.click (Options False False)
                    let up = (upvoteArrow,upvoteArrowClicks)
                        down = (downvoteArrow,downvoteArrowClicks)
                    return (up,down)
                return arrows
            return arrows
            

        listingLinkComponent = do
            child "a" Nothing $ do
                column 11
                addStyles
                    [ style "text-decoration" "none"
                    , style "font-size" "15px"
                    , style "font-family" "\"Helvetica Neue\",Arial"
                    , style "color" "teal"
                    ]
                setAttr "href" listingLink
                setText listingTitle

        voteCountComponent = do
            (_,(vc,vcSignal)) <- child "div" Nothing $ do
                column 1
                vcSignal <- construct undefined
                (count,_) <- child "div" Nothing $ setText $ show listingVotes
                return (count,vcSignal)
            return (vc,vcSignal)
            where
                shorten n
                    | n > 1000 = show (n `div` 1000) ++ "K"
                    | otherwise = show n

        bylineComponent = do
            (_,(_,ccSignal)) <- child "div" Nothing $ do
                column 11
                child "div" Nothing $ do
                    child "p" Nothing $ do
                        addStyles
                            [ style "display" "inline"
                            , style "font-size" "11px"
                            , style "font-family" "\"Times New Roman\",Serif"
                            , style "color" "gray"
                            ]
                        setText "Posted by: "
                    child "a" Nothing $ do
                        addStyles
                            [ style "display" "inline"
                            , style "text-decoration" "none"
                            , style "font-size" "11px"
                            , style "font-family" "\"Times New Roman\",Serif"
                            , style "color" "teal"
                            ]
                        setAttr "href" $ "#/user/" ++ listingAuthor
                        setText listingAuthor
                    child "p" Nothing $ do
                        addStyles
                            [ style "display" "inline"
                            , style "font-size" "11px"
                            , style "font-family" "\"Times New Roman\",Serif"
                            , style "color" "Black"
                            ]
                        setText " | "
                    child "a" Nothing $ do
                        addStyles
                            [ style "display" "inline"
                            , style "font-size" "11px"
                            , style "color" "teal"
                            , style "text-decoration" "none"
                            ]
                        setAttr "target" "_blank"
                        setAttr "href" ("http://" ++ shortSite listingLink)
                        setText (shortSite listingLink)
                    child "p" Nothing $ do
                        addStyles
                            [ style "display" "inline"
                            , style "font-size" "11px"
                            , style "font-family" "\"Times New Roman\",Serif"
                            , style "color" "black"
                            ]
                        setText " | "
                    (_,ccSignal) <- child "a" Nothing $ do
                        addStyles
                            [ style "text-decoration" "none"
                            , style "font-size" "11px"
                            , style "font-family" "\"Times New Roman\",Serif"
                            , style "color" "teal"
                            , style "margin-right" "8px"
                            ]
                        setAttr "href" $ "#/post/" ++ show listingShortcode
                        setText "comments (0)"
                        construct undefined
                    return ccSignal
            return ccSignal

shortSite ('h':'t':'t':'p':'s':':':'/':'/':'w':'w':'w':'.':lnk) = takeWhile (/= '/') lnk
shortSite ('h':'t':'t':'p':':':'/':'/':'w':'w':'w':'.':lnk) = takeWhile (/= '/') lnk
shortSite ('h':'t':'t':'p':'s':':':'/':'/':lnk) = takeWhile (/= '/') lnk
shortSite ('h':'t':'t':'p':':':'/':'/':lnk) = takeWhile (/= '/') lnk
shortSite lnk = takeWhile (/= '/') lnk
  
sampleListings =
    [ Listing "My girlfriend broke her foot 10 minutes\
              \ in to her graduation photo shoot"
              "Typic0le" 5561 768 [Tag "Funny" "green"]
              123456 "http://i.imgur.com/d61MtRD.jpg"
    , Listing "TIL Victorinox have never laid off an \
              \employee. To avoid this they set aside\
              \ profits during boom periods to supplement\
              \ any recession, they also temporarily\
              \ contract employees to other companies.\
              \ In 2009, Victorinox had over 100 employees\
              \ that had been with the company for more\
              \ than 40 years."
              "laanai_98" 7096 1070 [] 123457
              "http://www.sharpen-up.com/short-history-world-famous-swiss-army-knife/"
    , Listing "Office with a slide in central London"
              "eoec1995" 4262 332 [] 123458 "http://imgur.com/wCeO2eP"
    , Listing "When people make fun of me watching twitch\
              \ I show them this..." "cboyer21543" 2461 1294
              [] 123459 "http://www.loadingartist.com/wp-content/uploads/2015/09/2015-09-10-waste-of-time.jpg"
    , Listing "Hanging lake, Colorado. Photo by RattTrap Artistry [1024x768]"
              "RattTrapArtistry" 3209 163 [] 123460 "http://imgur.com/isf8JtZ"
    , Listing "Just watch the sci-fi comedy film Evolution (2001) and I\
              \ must say it still holds up after all these years, the cast\
              \ is pretty impressive, the CGI still holds up fantastically.\
              \ Give it a go sometime."
              "TheHandsomePhantom" 2506 344 [] 123461 "https://www.youtube.com/watch?v=qAiUZUHcEbQ"
    , Listing "Nick Offerman Used Wood From the Parks & Rec Set to Make Canoe\
              \ Paddles for His Parks & Rec Family"
              "lizzietishthefish" 185 32 [] 123462
              "http://www.vulture.com/2016/03/nick-offerman-parks-and-rec-canoe-paddle.html"
    , Listing "LPT: Write down the serial numbers of all of the expensive\
              \ items you own. If it gets stolen, it's much easier to prove\
              \ that the item is yours if it's found."
              "TheAlphaCarb0n" 4691 423 [] 123463
              "https://www.reddit.com/r/LifeProTips/comments/4bf9vy/lpt_write_down_the_serial_numbers_of_all_of_the/"
    ]

main :: IO ()
main = simple $ do
    let pg :: Maybe (Menu -> Node) -> Narrative '[Router] (Narrative Web IO) ()
        pg selector = do
            page $ void $ do
                child "div" Nothing $ do
                    addStyles [ style "min-height" "100%" ]
                    containerFluid
                    child "div" Nothing $ do
                        addStyles [ style "padding-bottom" "16px" ]
                        menu <- navbar
                        case selector of
                            Just sel -> mkActive (sel menu)
                            Nothing -> return ()
                        divider
                    (_,(clicks,_)) <- child "button" Nothing $ do
                        setText "Refresh"
                        listen E.click (Options False False)
                    live <- liftIO' newEmptyMVar
                    child "div" (Just "content") $ child "div" Nothing $
                      child "div" Nothing $ do
                                lis <- forM (zip [1..20] $ take 20 $ cycle sampleListings) $ \(n,l) -> do
                                    li <- listing l
                                    when (n /= 20) $ do
                                        void $ child "hr" Nothing $ do
                                            addStyles
                                                [ style "border" "0"
                                                , style "height" "0"
                                                , style "margin-right" "-16px"
                                                , style "margin-left" "-16px"
                                                , style "border-top" "1px solid rgba(0, 0, 0, 0.1)"
                                                , style "border-bottom" "1px solid rgba(255, 255, 255, 0.3)"
                                                ]
                                    return li
                                liftIO' $ putMVar live lis
                    Signaled gb <- super getSignalBuffer
                    super $ event $ \e -> (behavior e) clicks $ \_ _ -> do
                        void $ with "content" $ do
                            deleteChildren
                            lis <- liftIO' $ takeMVar live
                            forM_ lis $ \LiveListing{..} -> liftIO' $ do
                                arrive (unsafeCoerce gb) ([undefined],upvoteSignal)
                                arrive (unsafeCoerce gb) ([undefined],downvoteSignal)
                                arrive (unsafeCoerce gb)
                                  ([toEvent (Nothing :: Maybe (Int,Narrative '[HTML Web IO] (Narrative Web IO) ()))]
                                  ,voteCountSignal)
                            append "div" Nothing $ do
                              child "div" Nothing $ do
                                lis <- forM (zip [1..20] $ take 20 $ cycle sampleListings) $ \(n,l) -> do
                                    li <- listing l
                                    when (n /= 20) $ do
                                        void $ child "hr" Nothing $ do
                                            addStyles
                                                [ style "border" "0"
                                                , style "height" "0"
                                                , style "margin-right" "-16px"
                                                , style "margin-left" "-16px"
                                                , style "border-top" "1px solid rgba(0, 0, 0, 0.1)"
                                                , style "border-bottom" "1px solid rgba(255, 255, 255, 0.3)"
                                                ]
                                    return li
                                liftIO' $ putMVar live lis
                    footer
    path "/interesting" $ pg $ Just interesting
    path "/new" $ pg Nothing
    path "/provocative" $ pg $ Just provocative
    path "/login" $ pg Nothing
    pg $ Just provocative
