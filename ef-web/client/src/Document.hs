{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Document where

import Ef

import Element

import Data.List
import qualified Data.Map as Map


data Document = Document
    { root :: forall self super. Element self super
    , title :: String
    , url :: String
    , params :: Map.Map String String
    , cacheStrategy :: CacheDuration
    }



data CacheDuration
    = Priority Int
    | Permanent
    | Transient
instance Eq CacheDuration where
    (==) (Priority n) (Priority n') = n == n'
    (==) Permanent Permanent = True
    (==) Transient Transient = True
    (==) _ _ = False
instance Ord CacheDuration where
    compare Permanent Permanent = EQ
    compare Transient Transient = EQ
    compare (Priority n) (Priority n') = compare n n'
    compare Permanent _ = GT
    compare Transient _ = LT
    compare _ Permanent = LT
    compare _ Transient = GT



data CacheConfiguration = CacheConfiguration
    { keepDocuments :: Int
    }


data Documents k
    = Documents
          { activeDocument :: Document
          , activeDocumentGetter :: k
          , activeDocumentChanger :: Document -> k

          , cacheConfiguration :: CacheConfiguration
          , cacheConfigurationGetter :: k
          , cacheConfigurationSetter :: CacheConfiguration -> k

          , documentCache :: Map.Map String Document
          , documentCacheGetter :: k
          , documentCacheAdder :: Document -> k

          , documentCacheCleaner :: k
          }

    | GetActiveDocument (Document -> k)
    | ChangeActiveDocument Document k

    | GetCacheConfiguration (CacheConfiguration -> k)
    | SetCacheConfiguration CacheConfiguration k

    | GetDocumentCache (Map.Map String Document -> k)
    | AddToDocumentCache Document k

    | LookupURL String (Maybe Document -> k)


instance Ma Documents Documents where
    ma use Documents{..} (GetActiveDocument pk)       = use activeDocumentGetter (pk activeDocument)
    ma use Documents{..} (ChangeActiveDocument p k)   = use (activeDocumentChanger p) k
    ma use Documents{..} (GetDocumentCache ck)        = use documentCacheGetter (ck documentCache)
    ma use Documents{..} (AddToDocumentCache p k)     = use (documentCacheAdder p) k
    ma use Documents{..} (GetCacheConfiguration ck)   = use cacheConfigurationGetter (ck cacheConfiguration)
    ma use Documents{..} (SetCacheConfiguration cc k) = use (cacheConfigurationSetter cc) k


getActiveDocument = self (GetActiveDocument id)



activateDocument doc = self (ChangeActiveDocument doc ())



getDocumentCache = self (GetDocumentCache id)



loadDocument doc = self (AddToDocumentCache doc ())



getCacheConfiguration = self (GetCacheConfiguration id)



setCacheConfiguration cc = self (SetCacheConfiguration cc ())



lookupURL url = do
    cache <- getDocumentCache
    return $ Map.lookup url cache



defaultCacheConf = CacheConfiguration 3



documents seedDoc conf =
    Documents
        { activeDocument           = seedDoc
        , activeDocumentGetter     = return
        , activeDocumentChanger    = docChanger
        , cacheConfiguration       = conf
        , cacheConfigurationGetter = return
        , cacheConfigurationSetter = confSetter
        , documentCache            = Map.empty
        , documentCacheGetter      = return
        , documentCacheAdder       = cacheAdder
        , documentCacheCleaner     = cacheCleaner
        }
    where

        docChanger newDoc fs =
            let Documents{..} = view fs
            in case cacheStrategy activeDocument of
                   Transient -> return $ fs .= Documents{ activeDocument = newDoc, .. }
                   _ ->
                       let newCache = Map.insert (url newDoc) newDoc documentCache
                       in return $ fs .= Documents { activeDocument = newDoc, .. }

        confSetter newConf fs =
            let Documents{..} = view fs
            in return $ fs .= Documents { cacheConfiguration = newConf, .. }

        cacheAdder doc fs =
            let Documents{..} = view fs
                newCache = Map.insert (url doc) doc documentCache
            in return $ fs .= Documents { documentCache = newCache, .. }

        cacheCleaner fs =
            let Documents{..} = view fs
                keepCount = keepDocuments cacheConfiguration
                documents = Map.elems documentCache
                sortedDocuments = sortOn cacheStrategy documents
                newCacheDocuments = take keepCount sortedDocuments
                newCache = Map.fromList $ map (\doc -> (url doc,doc)) newCacheDocuments
            in return $ fs .= Documents { documentCache = newCache, .. }


