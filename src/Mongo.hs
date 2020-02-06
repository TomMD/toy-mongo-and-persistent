{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

import Database.Persist (insert, (==.), selectList, Entity(..), PersistField(..), PersistValue(PersistByteString))
import Database.Persist.Sql (PersistFieldSql(..))
import Database.Persist.TH
import Database.Persist.MongoDB (runMongoDBPool, withMongoDBConn, master, Action)
import Control.Monad.Cont (liftIO)
import MongoImport
import Database.MongoDB.Connection (PortID(PortNumber))
import Language.Haskell.TH (Type(..))

share
    [mkPersist mongoSettings]
    [persistLowerCase|
User
    name Name
    age Int Maybe
    deriving Show
Blogpost
    title String
    uid UserId
    UniqueUser uid
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
             (do user <- insert (User "John Doe" $ Just 35)
                 liftIO $ print user
                 vals <- selectList [ UserName ==. "John Doe" ] [] :: Action IO [Entity User]
                 liftIO $ print (map entityVal vals)
                 return ()))
