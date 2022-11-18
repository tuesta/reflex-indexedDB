module Reflex.IndexedDB
  ( HasDB(..), IsDB(..)
  , DB, RecDB
  , runDB, liftDB
  , key, fmodel, routeDB, routeDB', routeModel
  , KeyType(..), FromKey(..), ToKey(..)
  , Field
  , Model, select
  , Get(..), Getted(..), Put(..), Remove(..), Action(..), emptyA, runGetted
  , sequenceB

  , openDatabase
  , view
  , update
  , remove
  , getAll, getAllKeys
  ) where

import           Reflex.IndexedDB.Base
import           Reflex.IndexedDB.Class
