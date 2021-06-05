{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Bot.Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (unpack)
import Data.String (IsString (fromString))
import Database.Esqueleto.Experimental
import Database.Persist.Class (PersistField (fromPersistValue, toPersistValue))
import Generics.Deriving (Generic)

data DecisionStatus = Rejected | Accepted | Process
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

instance PersistField DecisionStatus where
  toPersistValue Rejected = PersistByteString "Rejected"
  toPersistValue Accepted = PersistByteString "Accepted"
  toPersistValue Process = PersistByteString "Process"

  fromPersistValue (PersistByteString "Rejected") = pure Rejected
  fromPersistValue (PersistByteString "Accepted") = pure Accepted
  fromPersistValue (PersistByteString "Process") = pure Process
  fromPersistValue _ = Left "is not a DecisionStatus"

instance PersistFieldSql DecisionStatus where
  sqlType _ = SqlBlob
