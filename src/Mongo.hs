{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RecordWildCards            #-}

import Database.Persist (insert, (>=.), (==.), selectList, Entity(..), PersistField(..), PersistValue(PersistByteString), (||.))
import Database.Persist.Sql (PersistFieldSql(..))
import Database.Persist.TH
import Database.Persist.MongoDB (runMongoDBPool, withMongoDBConn, master, Action
                                , (=:))
import Database.MongoDB.Admin (createIndex,index)
import Control.Monad.Cont (liftIO)
import Database.MongoDB.Connection (PortID(PortNumber))
import Language.Haskell.TH (Type(..))
import Data.Traversable
import Data.Foldable

import MongoImport

share
    [mkPersist mongoSettings]
    [persistLowerCase|

BThing
    anyInnerRecordWithCamelFieldsFails Int
    deriving Show

AThing
    record_field BThing
    deriving Show
|]



main :: IO ()
main =
    withMongoDBConn
        "myDatabaseName"
        "localhost"
        (PortNumber 27017)
        Nothing
        2000
        (runMongoDBPool
             master
             (do athing <- insert (AThing (BThing 1))
                 liftIO $ print athing
                 athingEnt <- selectList [] [] :: Action IO [Entity AThing]
                 liftIO $ print (map entityVal athingEnt)
                 return ()))
