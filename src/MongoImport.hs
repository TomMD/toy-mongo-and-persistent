{-# LANGUAGE TemplateHaskell #-}

module MongoImport where

import Language.Haskell.TH (Type(..))
import Database.Persist.TH
import Database.Persist.MongoDB

mongoSettings =
    (mkPersistSettings (ConT ''MongoContext))
    { mpsGeneric = False
    }
