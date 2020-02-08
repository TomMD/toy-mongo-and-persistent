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
User
    name Name
    age Age Maybe
    gender Person
    deriving Show
Blogpost
    title String
    pet Animal
    uid UserId
    UniqueUser uid
    deriving Show
|]

-- What about tables with rows that can represent success or failure of an
-- operation?
share
    [mkPersist mongoSettings]
    [persistLowerCase|
ResultPart
    resultMessage String
    deriving Show
ResultSuccess
    ident       Int
    yay1 ResultPartId
    yay2 ResultPartId
    deriving Show
ResultFailure
    ident  Int
    omgWTF String
    omgWhy String
    deriving Show
+Result
    f ResultFailure
    s ResultSuccess
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
                 _  <- insert (Blogpost "Does actually exist" (Cat (Age 34)) user)
                 bps <- selectList [] [] :: Action IO [Entity Blogpost]
                 liftIO (print bps)

                 y1 <- insert (ResultPart "yay1")
                 y2 <- insert (ResultPart "yay2")

                 _fId <- insert (ResultFailure 1 "oh no" "badness")
                 _sId <- insert (ResultSuccess 1 y1 y2)

                 -- N.B. We don't have a sum type even with the `+Result` so we
                 -- just query two collections.
                 resultRowSucc <- selectList [ ResultSuccessIdent ==. (1::Int) ] [] :: Action IO [Entity ResultSuccess]
                 resultRowf <- selectList [ ResultFailureIdent ==. (1::Int) ] [] :: Action IO [Entity ResultFailure]
                 liftIO (print (resultRowSucc, resultRowf))

                 -- Now how do we get a row from a related collection?
                 -- It seems we can lookup the row Id (ResultPartId) and, if
                 -- desired, reconstruct the original type or just use the field
                 -- separately as shown here where we print the ResultPart.
                 for_ resultRowSucc $ \s ->
                     do rps <- selectList ( [ ResultPartId ==. resultSuccessYay1 (entityVal s)  ] ||.
                                            [ ResultPartId ==. resultSuccessYay2 (entityVal s)] ) [] :: Action IO [Entity ResultPart]
                        liftIO (print (entityVal <$> rps))

                 return ()))
