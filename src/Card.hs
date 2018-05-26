{-# LANGUAGE DeriveGeneric #-}
module Card where

import           Data.Aeson   (FromJSON)
import           GHC.Generics (Generic)

data Card = Card
  { name  :: String
  , dbfId :: Int
  } deriving (Show, Generic)

instance FromJSON Card
