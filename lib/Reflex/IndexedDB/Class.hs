{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wno-orphans                #-}

module Reflex.IndexedDB.Class where

import           Data.Coerce
import           Data.Functor.Compose        (Compose, getCompose)
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.Product        (Product (..))
import           Data.Maybe                  (fromJust)
import           GHC.Generics
import           Text.Read                   (readMaybe)

import           Control.Monad.Primitive     (PrimMonad (PrimState), primitive)
import           Control.Monad.Reader
import           Control.Monad.Ref           (MonadRef (Ref), newRef, readRef,
                                              writeRef)
import           Control.Monad.Trans.Control (MonadTransControl (StT),
                                              defaultLiftWith, defaultRestoreT,
                                              liftWith, restoreT)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Sequence
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (UTCTime (..))
import           Data.Time.Clock.POSIX

import           Reflex.Dom.Core             (Adjustable (..), DomBuilder (..),
                                              DomRenderHook, HasDocument,
                                              MonadHold, MonadSample, NotReady,
                                              PerformEvent (..), PostBuild,
                                              Prerender (..), RawDocument,
                                              TriggerEvent, coerceEvent)
import           Reflex.Host.Class           (MonadReflexCreateTrigger)

import           GHCJS.DOM.IDBDatabase       (IDBDatabase)
import qualified GHCJS.DOM.Types             as DOM
import qualified Language.Javascript.JSaddle as JS

storeName, dbName :: String
storeName = "object_store"
dbName    = "database"

data KeyType
  = KeyString Text
  | KeyDate   UTCTime
  | KeyNum    Int
  deriving (Eq, Show)

pattern ModelId :: KeyType
pattern ModelId = KeyNum 0

data JSType
  = Undefined
  | Object
  | Boolean
  | Number
  | String
  | Symbol
  | Function
  | Other
  deriving (Enum)

jsTypeOf :: JS.JSVal -> JS.JSM JSType
jsTypeOf jsVal = fmap toEnum (JS.jsg1 ("h$jsTypeOf" :: T.Text) jsVal >>= DOM.fromJSValUnchecked)

instance JS.ToJSVal KeyType where
  toJSVal (KeyString str) = JS.toJSVal str
  toJSVal (KeyDate  date) = JS.toJSVal date
  toJSVal (KeyNum   fl)   = JS.toJSVal fl
instance JS.FromJSVal KeyType where
  fromJSVal jsVal = jsTypeOf jsVal >>= \case
    Number -> fmap KeyNum    <$> JS.fromJSVal @Int    jsVal
    String -> fmap KeyString <$> JS.fromJSVal @T.Text jsVal
    Object -> fmap KeyDate   <$> JS.fromJSVal @UTCTime jsVal
    _      -> pure Nothing

toMillis :: UTCTime -> Int
toMillis = floor @Double . (1e3 *) . realToFrac . utcTimeToPOSIXSeconds

fromMillis :: Int -> UTCTime
fromMillis t = posixSecondsToUTCTime $ (fromIntegral t) / 1000

instance JS.ToJSVal UTCTime where
  toJSVal = JS.new (JS.jsg ("Date" :: T.Text)) . (:[]) . toMillis
-- TODO this is unsafe
instance JS.FromJSVal UTCTime where
  fromJSVal jsVal = do
    timeStamp <- (jsVal JS.# ("getTime" :: T.Text) $ ()) >>= JS.fromJSVal @Int
    pure $ fromMillis <$> timeStamp

class ToKey k where
  toKey   :: k -> KeyType

class FromKey k where
  fromKey :: KeyType -> Maybe k

instance ToKey KeyType where
  toKey = id
instance FromKey KeyType where
  fromKey = pure

instance ToKey Text where
  toKey = KeyString
instance FromKey Text where
  fromKey (KeyString t) = pure t
  fromKey _             = Nothing

instance ToKey UTCTime where
  toKey = KeyDate
instance FromKey UTCTime where
  fromKey (KeyDate d) = pure d
  fromKey _           = Nothing

instance ToKey Int where
  toKey = KeyNum
instance FromKey Int where
  fromKey (KeyNum n) = pure n
  fromKey _          = Nothing

instance ToKey Char where
  toKey = toKey . T.pack . (:[])
instance FromKey Char where
  fromKey (KeyString c) | T.length c == 1 = pure (T.head c)
  fromKey _ = Nothing

instance ToKey Bool where
  toKey = toKey . T.pack . show
instance FromKey Bool where
  fromKey (KeyString b) = readMaybe @Bool (T.unpack b)
  fromKey _             = Nothing

instance ToKey Integer where
  toKey = toKey @Int . fromIntegral
instance FromKey Integer where
  fromKey = fmap fromIntegral . fromKey @Int

data Model key k m where
  Select  :: Maybe (Seq KeyType) -> Map key (k m) -> Model key k m

instance (IsDB k, ToKey key, Ord key) => IsDB (Model key k) where
  natTrans nt (Select ks mkm) = Select ks (natTrans nt <$> mkm)
  pureB _ = Select Nothing mempty
  traverseB f (Select ks mkm) = Select ks <$> traverse (traverseB f) mkm
  withField px (Select _ mkm) = Select (Just px) $
    Map.mapWithKey (\k -> withField (px |> toKey k)) mkm
  modifyDB f (Select ks mkm) =
    let ctxM k = fromJust ks |> ModelId |> toKey @key k
     in Map.foldrWithKey (\k sq acc -> sq >< (acc |> ctxM k)) mempty
     <$> traverse (modifyDB f) mkm

select :: Map key (k m) -> Model key k m
select = Select Nothing

data Getted a = NoGetted | Getted a
  deriving (Eq, Ord, Show)

data Get a where
  NoGet :: Get a
  Get   :: FromJSON a => Get a

data Put a where
  NoPut :: Put a
  Put   :: ToJSON a => a -> Put a

data Remove a = Remove | NoRemove

class Action f where
  noAction :: f a
instance Action Get where
  noAction = NoGet
instance Action Put where
  noAction = NoPut
instance Action Remove where
  noAction = NoRemove

emptyA :: (IsDB k, Action f) => k f
emptyA = pureB noAction

runGetted :: IsDB k => k Getted -> Maybe (k Identity)
runGetted = traverseB $ \case
  NoGetted -> Nothing
  Getted g -> Just (pure g)

class IsDB k where
  natTrans :: (forall a. f a -> g a) -> k f -> k g
  default natTrans
    :: ( Generic (k f), Generic (k g)
       , GNatTrans f g (Rep (k f)) (Rep (k g))
       )
    => (forall a. f a -> g a) -> k f -> k g
  natTrans nt = to . gnatTrans nt . from
  pureB :: (forall a. (ToJSON a, FromJSON a) => f a) -> k f
  default pureB
    :: ( Generic (k f)
       , GApplicative f (Rep (k f))
       )
    => (forall a. (ToJSON a, FromJSON a) => f a) -> k f
  pureB f = to (gpure f)
  traverseB
    :: Applicative t
    => (forall a. ToJSON a => f a -> t (g a))
    -> k f
    -> t (k g)
  default traverseB
    :: ( Generic (k f), Generic (k g)
       , GTraversable f g (Rep (k f)) (Rep (k g))
       , Applicative t
       )
    => (forall a. ToJSON a => f a -> t (g a))
    -> k f -> t (k g)
  traverseB f = fmap to . gtraverse f . from
  withField :: Seq KeyType -> k f -> k (Field `Product` f)
  default withField
    :: ( Generic (k f), Generic (k (Field `Product` f))
       , GField (Rep (k f)) (Rep (k (Field `Product` f)))
       )
    => Seq KeyType -> k f -> k (Field `Product` f)
  withField px = to . gfield px . from
  modifyDB
    :: Applicative t
    => (forall a. Seq KeyType -> f a -> t ())
    -> k (Field `Product` f)
    -> t (Seq (Seq KeyType))
  default modifyDB
    :: ( Generic (k (Field `Product` f))
       , GModifyDB f (Rep (k (Field `Product` f)))
       , Applicative t
       )
    => (forall a. Seq KeyType -> f a -> t ())
    -> k (Field `Product` f)
    -> t (Seq (Seq KeyType))
  modifyDB f = gmodifyDB f . from

class (IsDB k, Monad m) => HasDB k m where
  askDB :: m (IDBDatabase, Seq KeyType)

instance (IsDB k, Monad m) => HasDB k (DB k m) where
  askDB = DB ask

newtype DB k m a = DB { getDB :: ReaderT (IDBDatabase, Seq KeyType) m a }
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans, DOM.MonadJSM
           , MonadSample t, MonadHold t, PostBuild t, NotReady t, TriggerEvent t
           , DomRenderHook t, MonadReflexCreateTrigger t
           )

instance MonadTransControl (DB k) where
  type StT (DB k) a = StT (ReaderT (IDBDatabase, Seq KeyType)) a
  liftWith = defaultLiftWith DB getDB
  restoreT = defaultRestoreT DB

instance PerformEvent t m => PerformEvent t (DB k m) where
  type Performable (DB k m) = Performable m
  performEvent  = lift . performEvent
  performEvent_ = lift . performEvent_

instance DomBuilder t m => DomBuilder t (DB k m) where
  type DomBuilderSpace (DB k m) = DomBuilderSpace m
instance Adjustable t m => Adjustable t (DB k m) where
  runWithReplace a0 a' = DB $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = DB $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = DB $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = DB $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance MonadRef m => MonadRef (DB k m) where
  type Ref (DB k m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance
  ( Monad m
  , RawDocument (DomBuilderSpace (DB k m)) ~ RawDocument (DomBuilderSpace m)
  , MonadTrans (DB k)
  , HasDocument m
  ) => HasDocument (DB k m)


instance PrimMonad m => PrimMonad (DB k m) where
  type PrimState (DB k m) = PrimState m
  primitive = lift . primitive

instance (Prerender t m, Monad m, DomRenderHook t (DB k (Client m))) => Prerender t (DB k m) where
  type Client (DB k m) = DB k (Client m)
  prerender server client = DB $ do
    env <- ask
    lift $ prerender
      (runReaderT (getDB server) env)
      (runReaderT (getDB client) env)

type RecDB k m = HasDB k (DB k m)

runDB :: (IDBDatabase, Seq KeyType) -> DB k m a -> m a
runDB r (DB db) = runReaderT db r

liftDB :: forall k m a. HasDB k m => DB k m a -> m a
liftDB (DB m) = askDB @k @m >>= runReaderT m

mgetContext :: Model key k f -> Maybe (Seq KeyType)
mgetContext (Select ctx _) = ctx

getContext :: Model key k Field -> Seq KeyType
getContext = fromJust . mgetContext

key :: ToKey key => key -> Model key k Field -> Field (k Field)
key k m = Field $ getContext m |> toKey k

fmodel :: Model key k Field -> Field (Model key k Field)
fmodel m = Field $ getContext m

emptyB :: IsDB k => k (Const ())
emptyB = pureB $ Const ()

firstB :: IsDB k => k (f `Product` g) -> k f
firstB = natTrans $ \(Pair f _) -> f

routeDB :: forall k' k m a. (IsDB k, HasDB k m) => (k Field -> Field (k' Field)) -> DB k' m a -> m a
routeDB route (DB m) = liftDB @k @m $ DB $ ReaderT $ \(db, ctx) ->
  runReaderT m (db, unField . route . firstB $ withField ctx emptyB)

routeDB' :: IsDB k => (k Field -> Field (k' Field)) -> DB k' m a -> DB k m a
routeDB' route (DB m) = DB $ ReaderT $ \(db, ctx) ->
  runReaderT m (db, unField . route . firstB $ withField ctx emptyB)

routeModel
  :: forall k' k m key a
   . (IsDB k, HasDB k m, FromKey key)
  => (k Field -> Model key k' Field)
  -> DB (Model key k') m a
  -> m a
routeModel route (DB m) = liftDB @k @m $ DB $ ReaderT $ \(db, ctx) ->
  runReaderT m (db, getContext . route . firstB $ withField ctx emptyB)

-- TODO add support for sum types

------------------------------------------------------------------------------
-- Field
------------------------------------------------------------------------------

newtype Field a = Field
  { unField :: Seq KeyType
  }

class GField i o where
  gfield :: Seq KeyType -> i p -> o p

instance (Datatype meta1, GField i o) => GField (D1 meta1 i) (D1 meta2 o) where
  gfield px d@(M1 k) = M1 $ gfield (px |> kt) k
    where
      kt = KeyString $ T.pack $ moduleName d <> "." <> datatypeName d

instance GField i o => GField (C1 _m1 i) (C1 _m2 o) where
  gfield px (M1 k) = M1 $ gfield px k

instance (GField i o, GField i' o') => GField (i :*: i') (o :*: o') where
  gfield px (l :*: r) = gfield px l :*: gfield px r

instance (Selector meta1, GField i o) => GField (S1 meta1 i) (S1 meta2 o) where
  gfield px s@(M1 k) = M1 $ gfield (px |> kt) k
    where
      kt = KeyString $ T.pack $ selName s

instance GField (K1 a1 (f k)) (K1 a2 (Product Field f k)) where
  gfield px (K1 f) = K1 $ Pair (Field px) f

instance
  ( Generic (k f), Generic (k (Field `Product` f))
  , GField (Rep (k f)) (Rep (k (Field `Product` f)))
  , FromKey key, ToKey key
  )
  => GField (K1 a1 (Model key k f)) (K1 a2 (Model key k (Field `Product` f))) where
  gfield px (K1 (Select _ mkm)) = K1 $ Select (Just px) (Map.mapWithKey with mkm)
    where
      with :: key -> k f -> k (Field `Product` f)
      with k = to . gfield (px |> toKey k) . from

------------------------------------------------------------------------------
-- ModifyDB
------------------------------------------------------------------------------

class GModifyDB f i where
  gmodifyDB
    :: Applicative t
    => (forall a. Seq KeyType -> f a -> t ())
    -> i p
    -> t (Seq (Seq KeyType))

instance GModifyDB f i => GModifyDB f (M1 _a1 _b1 i) where
  gmodifyDB f (M1 k) = gmodifyDB f k

instance (GModifyDB f i, GModifyDB f i')
  => GModifyDB f (i :*: i') where
  gmodifyDB f (l :*: r) = (><) <$> gmodifyDB f l <*> gmodifyDB f r

instance GModifyDB f (K1 a1 (Product Field f k)) where
  gmodifyDB f (K1 (Pair (Field path) k)) = f path k *> pure mempty

instance
  ( Generic (k (Field `Product` f))
  , GModifyDB f (Rep (k (Field `Product` f)))
  , Ord key, ToKey key
  )
  => GModifyDB f (K1 a (Model key k (Field `Product` f))) where
  gmodifyDB f (K1 (Select ks mkm)) =
    let ctxM k    = fromJust ks |> ModelId |> toKey @key k
        rmodifyDB = gmodifyDB f . from
     in  Map.foldrWithKey (\k sq acc -> sq >< (acc |> ctxM k)) mempty
     <$> traverse rmodifyDB mkm

------------------------------------------------------------------------------
-- Natural Transformation
------------------------------------------------------------------------------

class GNatTrans f g i o where
  gnatTrans :: (forall a. f a -> g a) -> i p -> o p

instance GNatTrans f g i o => GNatTrans f g (M1 _a _b i) (M1 _a _b o) where
  gnatTrans nt (M1 k) = M1 (gnatTrans nt k)
instance (GNatTrans f g i o, GNatTrans f g i' o') => GNatTrans f g (i :*: i') (o :*: o') where
  gnatTrans nt (l :*: r) = gnatTrans nt l :*: gnatTrans nt r
instance GNatTrans f g (K1 a (f k)) (K1 a (g k)) where
  gnatTrans nt (K1 k) = K1 (nt k)
instance
  ( Generic (k f), Generic (k g)
  , GNatTrans f g (Rep (k f)) (Rep (k g))
  )
  => GNatTrans f g (K1 a (Model key k f)) (K1 a (Model key k g)) where
  gnatTrans nt (K1 (Select ks mkm)) = K1 $ Select ks (to . gnatTrans nt . from <$> mkm)

------------------------------------------------------------------------------
-- GApplicative
------------------------------------------------------------------------------

class GApplicative f o where
  gpure :: (forall a. (ToJSON a, FromJSON a) => f a) -> o p

instance GApplicative f o => GApplicative f (M1 _a _b o) where
  gpure f = M1 (gpure f)

instance (GApplicative f o, GApplicative f o') => GApplicative f (o :*: o') where
  gpure f = gpure f :*: gpure f

instance (FromJSON k, ToJSON k) => GApplicative f (K1 a (f k)) where
  gpure f = K1 f

instance
  ( Generic (k f), GApplicative f (Rep (k f))
  , Ord key
  ) => GApplicative f (K1 a (Model key k f)) where
  gpure _ = K1 $ Select Nothing mempty

------------------------------------------------------------------------------
-- TraversableB
------------------------------------------------------------------------------

class GTraversable f g i o where
  gtraverse
    :: Applicative t
    => (forall a. ToJSON a => f a -> t (g a))
    -> i p
    -> t (o p)

instance GTraversable f g i o => GTraversable f g (M1 _a _b i) (M1 _a _b o) where
  gtraverse f (M1 k) = M1 <$> gtraverse f k
instance (GTraversable f g i o, GTraversable f g i' o')
  => GTraversable f g (i :*: i') (o :*: o') where
  gtraverse f (l :*: r) = (:*:) <$> gtraverse f l <*> gtraverse f r
instance ToJSON k => GTraversable f g (K1 a1 (f k)) (K1 a2 (g k)) where
  gtraverse f (K1 k) = K1 <$> f k
instance
  ( Generic (k f), Generic (k g)
  , GTraversable f g (Rep (k f)) (Rep (k g))
  , Ord key
  )
  => GTraversable f g (K1 a (Model key k f)) (K1 a (Model key k g)) where
  gtraverse f (K1 (Select ks mkm)) =
    K1. Select ks <$> traverse (fmap to . gtraverse f . from) mkm

sequenceB :: (IsDB k, Applicative f) => k (Compose f g) -> f (k g)
sequenceB = traverseB getCompose
