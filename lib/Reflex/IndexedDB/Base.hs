{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS -Wno-orphans         #-}

module Reflex.IndexedDB.Base where

import           Control.Monad                (void)
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Functor.Product         (Product (..))

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Monad.Reader
import qualified Data.Aeson                   as Aeson
import           Data.Sequence                (Seq (..), (|>))
import qualified Data.Sequence                as Seq
import qualified Data.Text                    as T

import           Reflex.Dom.Core              hiding (Delete)

import qualified GHCJS.DOM                    as DOM
import qualified GHCJS.DOM.Enums              as DOM
import qualified GHCJS.DOM.EventM             as DOMEv
import qualified GHCJS.DOM.IDBCursor          as IDBCursor
import qualified GHCJS.DOM.IDBDatabase        as IDBDatabase
import qualified GHCJS.DOM.IDBFactory         as IDB
import qualified GHCJS.DOM.IDBKeyRange        as IDBKeyRange
import qualified GHCJS.DOM.IDBObjectStore     as IDBObjectStore
import qualified GHCJS.DOM.IDBOpenDBRequest   as IDBRequest
import qualified GHCJS.DOM.IDBRequest         as IDBRequest
import qualified GHCJS.DOM.IDBTransaction     as IDBTransaction
import qualified GHCJS.DOM.Types              as DOM
import qualified GHCJS.DOM.Window             as DOM
import qualified Language.Javascript.JSaddle  as JS

import           Reflex.IndexedDB.Class

openDatabase
  :: ( DOM.MonadJSM m
     , TriggerEvent t m
     , Reflex t
     )
  => m (Event t (Maybe DOM.IDBDatabase))
openDatabase = do
  window    <- DOM.currentWindowUnchecked
  indexedDB <- DOM.getIndexedDB window

  openRequest <- IDB.open indexedDB dbName Nothing

  (openSuccessEv, openSuccessTrigger) <- newTriggerEvent
  (openErrorEv  , openErrorTrigger)   <- newTriggerEvent

  -- IDBOpenDBRequest: error event
  void $ DOM.liftJSM $ DOMEv.on openRequest IDBRequest.error $ liftIO $ openErrorTrigger Nothing

  -- IDBOpenDBRequest: upgradeneeded event
  void $ DOM.liftJSM $ DOMEv.on openRequest IDBRequest.upgradeNeeded $ do
    Just target        <- DOMEv.target @_ @DOM.IDBOpenDBRequest
    Just requestResult <- IDBRequest.getResult target
    db <- DOM.unsafeCastTo IDBDatabase.IDBDatabase requestResult
    IDBDatabase.createObjectStore_ db storeName Nothing

  -- IDBRequest: success even
  void $ DOM.liftJSM $ DOMEv.on openRequest IDBRequest.success $ do
    Just target        <- DOMEv.target @_ @DOM.IDBOpenDBRequest
    Just requestResult <- IDBRequest.getResult target
    db <- DOM.unsafeCastTo IDBDatabase.IDBDatabase requestResult
    liftIO $ openSuccessTrigger (Just db)

  pure $ leftmost [openSuccessEv, openErrorEv]

update
  :: forall k m
   . ( DOM.MonadJSM m
     , IsDB k, HasDB k m
     )
  => k Put -> m () --(Event t Bool)
update k = askDB @k @m >>= \(db, ctx) -> do
  tx    <- IDBDatabase.transaction db [storeName] (Just DOM.IDBTransactionModeReadwrite)
  store <- IDBTransaction.objectStore tx storeName

  let updateField path = \case
        NoPut -> pure ()
        Put a -> IDBObjectStore.put_ store (Aeson.toJSON a) (Just $ toList path)

  kss <- modifyDB updateField $ withField ctx k

  -- (isSuccessEv, isSuccessTrigger) <- newTriggerEvent
  void $ DOM.liftJSM $ DOMEv.on tx IDBTransaction.error $
    pure ()
    -- liftIO $ isSuccessTrigger False
  void $ DOM.liftJSM $ DOMEv.on tx IDBTransaction.complete $ do
    tx'    <- IDBDatabase.transaction db [storeName] (Just DOM.IDBTransactionModeReadwrite)
    store' <- IDBTransaction.objectStore tx' storeName
    mapM_ (IDBObjectStore.put_ store' () . Just . toList) kss
    -- liftIO $ isSuccessTrigger True
    pure ()

  pure () -- isSuccessEv

removeIfEmpty :: DOM.JSM () -> DOM.IDBObjectStore -> Seq KeyType -> DOM.JSM ()
removeIfEmpty cl store pathM@(ctx :|> ModelId :|> kt) = do
  range <- IDBKeyRange.lowerBound (toList $ ctx |> kt) False
  req   <- IDBObjectStore.openCursorRange store (Just range) (Just DOM.IDBCursorDirectionNext)

  let deleteCl = do
        req' <- IDBObjectStore.delete store (toList pathM)
        void $ DOM.liftJSM $ DOMEv.on req' IDBRequest.success $ ReaderT (const cl)

  void $ DOM.liftJSM $ DOMEv.on req IDBRequest.success $ do
    Just target <- DOMEv.target @_ @DOM.IDBRequest
    mrequestResult <- IDBRequest.getResult target
    case mrequestResult of
      Just (DOM.IDBRequestResult requestResult) -> do
        keyJSVal <- DOM.liftJSM $ do
          keyJS <- requestResult JS.! ("key" :: T.Text)
          keyJS JS.# ("slice" :: T.Text) $ [0, Seq.length ctx + 1]
        mkeyType <- DOM.liftJSM (DOM.fromJSVal @[KeyType] keyJSVal)
        case mkeyType of
          Just ks | ks == toList (ctx |> kt) -> pure ()
          _ -> deleteCl
      _ -> deleteCl
removeIfEmpty cl _ _ = cl

remove
  :: forall k m
   . ( DOM.MonadJSM m
     , IsDB k, HasDB k m
     )
  => k Remove -> m ()
remove k = askDB @k @m >>= \(db, ctx) -> do
  tx    <- IDBDatabase.transaction db [storeName] (Just DOM.IDBTransactionModeReadwrite)
  store <- IDBTransaction.objectStore tx storeName

  let removeField path = \case
        NoRemove -> pure ()
        Remove   -> IDBObjectStore.delete_ store (toList path)

  kss <- modifyDB removeField $ withField ctx k

  void $ DOM.liftJSM $ DOMEv.on tx IDBTransaction.error $ pure ()
  void $ DOM.liftJSM $ DOMEv.on tx IDBTransaction.complete $ do
    tx'    <- IDBDatabase.transaction db [storeName] (Just DOM.IDBTransactionModeReadwrite)
    store' <- IDBTransaction.objectStore tx' storeName
    ReaderT $ \_ -> foldr (\sq acc -> removeIfEmpty acc store' sq) (pure ()) kss

  pure ()

viewField
  :: forall m a
   . DOM.MonadJSM m
  => DOM.IDBObjectStore
  -> Product Field Get a
  -> m (TMVar (Compose Maybe Getted a))
viewField objStore (Pair (Field path) g) = do
  tvar <- liftIO $ atomically $ newEmptyTMVar
  case g of
    NoGet -> liftIO . atomically . putTMVar tvar $ Compose (Just NoGetted)
    Get   -> do
      req <- IDBObjectStore.get objStore (toList path)
      void $ DOM.liftJSM $ DOMEv.on req IDBRequest.success $ do
        Just target    <- DOMEv.target @_ @DOM.IDBRequest
        mrequestResult <- IDBRequest.getResult target
        ma <- case mrequestResult of
          Nothing -> pure Nothing
          Just (DOM.IDBRequestResult requestResult) -> do
            mvalue <- DOM.liftJSM $ DOM.fromJSVal @Aeson.Value requestResult
            pure $ maybe Nothing (fmap Getted . toMaybe . Aeson.fromJSON @a) mvalue
        liftIO . atomically . putTMVar tvar $ Compose ma
  pure tvar

toMaybe :: Aeson.Result a -> Maybe a
toMaybe = \case
  Aeson.Error   _ -> Nothing
  Aeson.Success a -> Just a

view
  :: forall k t m
   . ( DOM.MonadJSM m, TriggerEvent t m
     , IsDB k, HasDB k m
     )
  => k Get
  -> m (Event t (k (Compose Maybe Getted)))
view k = askDB @k @m >>= \(db, ctx) -> do
  tx     <- IDBDatabase.transaction db [storeName] (Just DOM.IDBTransactionModeReadonly)
  store  <- IDBTransaction.objectStore tx storeName
  kIORef <- traverseB (fmap Compose . viewField store) $ withField ctx k

  (valueEv, valueTrigger) <- newTriggerEvent
  void $ DOM.liftJSM $ DOMEv.on tx IDBTransaction.error $
    liftIO . valueTrigger $ pureB (Compose Nothing)
  void $ DOM.liftJSM $ DOMEv.on tx IDBTransaction.complete $
    traverseB (liftIO . atomically . readTMVar . getCompose) kIORef >>= liftIO . valueTrigger

  pure valueEv

getAll
  :: forall key k t m
   . ( DOM.MonadJSM m, TriggerEvent t m
     , PerformEvent t m, MonadIO (Performable m)
     , IsDB k, HasDB (Model key k) m
     , Ord key, FromKey key, Show key
     )
  => k Get -> m (Event t (key, k (Compose Maybe Getted)))
getAll k = askDB @(Model key k) @m >>= \(db, ctx) -> do
  tx     <- IDBDatabase.transaction db [storeName] (Just DOM.IDBTransactionModeReadonly)
  store  <- IDBTransaction.objectStore tx storeName

  let ctxM = toList $ ctx |> ModelId

  range <- IDBKeyRange.lowerBound ctxM False
  req   <- IDBObjectStore.openCursorRange store (Just range) (Just DOM.IDBCursorDirectionNext)

  (valueEv, valueTrigger) <- newTriggerEvent
  void $ DOM.liftJSM $ DOMEv.on req IDBRequest.success $ do
    Just target <- DOMEv.target @_ @DOM.IDBRequest
    mrequestResult <- IDBRequest.getResult target
    case mrequestResult of
      Just (DOM.IDBRequestResult requestResult) -> do
        (ctxJS, ktJS) <- DOM.liftJSM $ do
          keyJS <- requestResult JS.! ("key" :: T.Text)
          ctxJS <- keyJS JS.# ("slice"  :: T.Text) $ [0, Seq.length ctx + 2]
          ktJS  <- ctxJS JS.# ("splice" :: T.Text) $ [-1 :: Int]
          pure (ctxJS, ktJS)
        (mctx, mkt) <- DOM.liftJSM $
          (,) <$> DOM.fromJSVal @[KeyType] ctxJS
              <*> DOM.fromJSVal @[KeyType] ktJS
        case (mctx, mkt) of
          (Just ctx', Just [kt]) | ctx' == ctxM -> do
            kIORef <- traverseB (fmap Compose . viewField store) $ withField (ctx |> kt) k
            liftIO $ maybe (pure ()) (valueTrigger . (, kIORef)) (fromKey @key kt)
            IDBCursor.continue (IDBCursor.IDBCursor requestResult) (Nothing :: Maybe DOM.JSVal)
          _ -> pure ()
      _ -> pure ()

  mvalueEv <- performEvent $ ffor valueEv $ \(ky, kIORef) ->
    (ky,) <$> traverseB (liftIO . atomically . readTMVar . getCompose) kIORef

  pure mvalueEv

getAllKeys
  :: forall key k t m
   . ( DOM.MonadJSM m, TriggerEvent t m, Reflex t
     , HasDB (Model key k) m, FromKey key
     )
  =>  m (Event t key)
getAllKeys = askDB @(Model key k) @m >>= \(db, ctx) -> do
  tx    <- IDBDatabase.transaction db [storeName] (Just DOM.IDBTransactionModeReadonly)
  store <- IDBTransaction.objectStore tx storeName

  let ctxM = toList (ctx |> ModelId)

  range <- IDBKeyRange.lowerBound ctxM False
  req   <- IDBObjectStore.openCursorRange store (Just range) (Just DOM.IDBCursorDirectionNext)

  (keyEv, keyTrigger) <- newTriggerEvent
  void $ DOM.liftJSM $ DOMEv.on req IDBRequest.success $ do
    Just target <- DOMEv.target @_ @DOM.IDBRequest
    mrequestResult <- IDBRequest.getResult target
    case mrequestResult of
      Just (DOM.IDBRequestResult requestResult) -> do
        (ctxJS, ktJS) <- DOM.liftJSM $ do
          keyJS <- requestResult JS.! ("key" :: T.Text)
          ctxJS <- keyJS JS.# ("slice"  :: T.Text) $ [0, Seq.length ctx + 2]
          ktJS  <- ctxJS JS.# ("splice" :: T.Text) $ [-1 :: Int]
          pure (ctxJS, ktJS)
        (mctx, mkt) <- DOM.liftJSM $
          (,) <$> DOM.fromJSVal @[KeyType] ctxJS
              <*> DOM.fromJSVal @[KeyType] ktJS
        case (mctx, mkt) of
          (Just ctx', Just [kt]) | ctx' == ctxM -> do
            liftIO $ maybe (pure ()) keyTrigger (fromKey kt)
            IDBCursor.continue (IDBCursor.IDBCursor requestResult) (Nothing @DOM.JSVal)
          _ -> pure ()
      _ -> pure ()

  pure keyEv
