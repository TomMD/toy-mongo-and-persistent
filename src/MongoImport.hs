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

mongoSettings =
    (mkPersistSettings (ConT ''MongoContext))
    { mpsGeneric = False
    }
