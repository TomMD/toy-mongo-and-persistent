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
import Database.Persist.MongoDB (runMongoDBPool, withMongoDBConn, master, Action
                                , (=:))
import Database.MongoDB.Admin (createIndex,index)
import Control.Monad.Cont (liftIO)
import MongoImport
import Database.MongoDB.Connection (PortID(PortNumber))
import Language.Haskell.TH (Type(..))

share
    [mkPersist mongoSettings]
    [persistLowerCase|
User
    name Name
    age Age Maybe
    gender Person
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
             (do user <- insert (User "John Doe" (Just (Age 35)) Male)
                 liftIO $ print user
                 vals <- selectList [ UserName ==. "John Doe" ] [] :: Action IO [Entity User]
                 liftIO $ print (map entityVal vals)
                 createIndex $ index "Blogpost" [ "title" =: (1 :: Int) {- accending -}
                                                , "uid"   =: (negate 1 :: Int) {- decending -} ]
                 bps <- selectList [ BlogpostTitle ==. "D.N.E." ] [] :: Action IO [Entity Blogpost]
                 liftIO (print bps)
                 return ()))
