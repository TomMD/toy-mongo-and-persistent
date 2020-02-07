{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}

module MongoImport where

import Data.Int
import Language.Haskell.TH (Type(..))
import Database.Persist.TH
import Database.Persist.MongoDB
import Database.Persist (insert, (==.), selectList, Entity(..), PersistField(..), PersistValue(PersistByteString, PersistInt64))
import Database.Persist.Sql (PersistFieldSql(..))
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Text as T
import Data.String
import qualified Data.Attoparsec.Text as P

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

-- Here we are demonstrating a sum type that is a single field of a larger row
-- going into MongoDB.
data Animal = Cat { catAge :: Age } | Dog { dogName :: Name, dogAge :: Age }
    deriving (Eq, Ord, Show)

-- All fields need an SQL type even though we aren't using SQL.
instance PersistFieldSql Animal where
    sqlType _ = SqlOther (T.pack "Animal")

-- Our way of supporting _this_ field is to use Text (vs ByteString for Name or
-- Int64 for age as above) and having our own serialization format and parser.
--
-- Format:   cat ##      ~~> ##
--           dog NAME ## ~~> ##:NAME
instance PersistField Animal where
  toPersistValue (Cat (Age a)) = PersistText (T.pack (show a))
  toPersistValue (Dog n (Age a)) = PersistText (T.pack (show a) <> ":" <> T.pack (show n))

  fromPersistValue (PersistText field) =
    case T.breakOn ":" field of
        (P.parseOnly P.decimal -> Right age, "") -> Right $ Cat (Age age)
        (P.parseOnly P.decimal -> Right ageInt,T.drop 1 -> nameText) ->
            Right $ Dog (Name nameText) (Age ageInt)
        (P.parseOnly P.decimal -> Left err, _) -> Left (T.pack err)
