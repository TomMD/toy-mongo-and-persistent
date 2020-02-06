{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MongoImport where

import Language.Haskell.TH (Type(..))
import Database.Persist.TH
import Database.Persist.MongoDB
import Database.Persist (insert, (==.), selectList, Entity(..), PersistField(..), PersistValue(PersistByteString))
import Database.Persist.Sql (PersistFieldSql(..))
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.String

-- |An example of a custom type being stored in MongoDB with persistent
-- automatically handling the (de)serialization for us.
newtype Name = Name T.Text deriving (Eq, Ord, Show, IsString)

instance PersistFieldSql Name where
    sqlType _ = SqlOther (T.pack "UserName")

instance PersistField Name where
  toPersistValue (Name n) = PersistByteString $ T.encodeUtf8 n
  fromPersistValue (PersistByteString name) =
    case T.decodeUtf8' name of
        Left e -> Left (T.pack $ show e)
        Right r -> Right (Name r)

mongoSettings =
    (mkPersistSettings (ConT ''MongoContext))
    { mpsGeneric = False
    }
