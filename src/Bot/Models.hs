{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (unpack)
import Data.String (IsString (fromString))
import Database.Esqueleto (PersistField, PersistFieldSql (sqlType), PersistValue (PersistByteString, PersistInt64), SqlType (SqlBlob), val)
import Database.Persist.Class (PersistField (fromPersistValue, toPersistValue))
import Generics.Deriving (Generic)

data DecisionStatus = Rejected | Accepted | Process deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance PersistField DecisionStatus where
  toPersistValue = PersistByteString . fromString . show
  fromPersistValue (PersistByteString s) = read $ unpack s

instance PersistFieldSql DecisionStatus where
  sqlType _ = SqlBlob
