{-# LANGUAGE DeriveGeneric #-}

module Onchain.HydraType where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HM
import Data.Swagger (ToSchema)

data UTxO = UTxO
  { txIn :: Text
  , address :: Text
  , value :: HM.HashMap Text Integer
  } deriving (Show, Generic)

instance ToJSON UTxO
instance FromJSON UTxO
instance ToSchema UTxO

data CommitRequest = CommitRequest
  { tokenId :: Text
  , amount :: Integer
  } deriving (Show, Generic)

instance ToJSON CommitRequest
instance FromJSON CommitRequest
instance ToSchema CommitRequest

-- data UpdateRequest = UpdateRequest
--   { tokenId :: Text
--   , newMetadata :: Text -- IPFS CID or JSON
--   } deriving (Show, Generic)

-- instance ToJSON UpdateRequest
-- instance FromJSON UpdateRequest
-- instance ToSchema UpdateRequest

-- data DecommitRequest = DecommitRequest
--   { tokenId :: Text
--   } deriving (Show, Generic)

-- instance ToJSON DecommitRequest
-- instance FromJSON DecommitRequest
-- instance ToSchema DecommitRequest
