{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MongoImport where

import Data.Int
import Language.Haskell.TH (Type(..))
import Database.Persist.TH
import Database.Persist.MongoDB
import Database.Persist (insert, (==.), selectList, Entity(..), PersistField(..), PersistValue(PersistByteString, PersistInt64))
import Database.Persist.Sql (PersistFieldSql(..))
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.String

-- |An example of a custom type being stored in MongoDB with persistent
-- automatically handling the (de)serialization for us.
newtype Name = Name T.Text deriving (Eq, Ord, Show, IsString)
newtype Age  = Age Int64 deriving (Eq, Ord, Show)

instance PersistFieldSql Age where
    sqlType _ = SqlOther (T.pack "Age")

instance PersistField Age where
  toPersistValue (Age n) = PersistInt64 n
  fromPersistValue (PersistInt64 age) = Right (Age age)

-- |Example of deriving persistent fields
data Person = Male | Female | Other
  deriving (Eq, Show, Read)
derivePersistField "Person"

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
