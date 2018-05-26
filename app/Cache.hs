{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Cache where

import           Hearthstone.Card

import           Control.Monad          (unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson             (FromJSON, ToJSON (..), defaultOptions,
                                         genericToEncoding)
import           Data.ByteString.Char8  (unpack)
import qualified Data.ByteString.Lazy   as ByteString
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.List              as List
import           Data.Serialize         (Serialize (..), decodeLazy, encodeLazy)
import           GHC.Generics           (Generic)
import           Network.HTTP.Conduit   (Request (..))
import           Network.HTTP.Simple
import           System.Directory

data Cache = Cache
  { build :: Int
  , cards :: IntMap Card
  } deriving (Show, Generic)

instance Serialize Cache

data CacheException =
    BadStatusCode Int
  | BadLocation String
  | DeserializeFailure String
  | NoLocInResponse
  deriving Show

instance Exception CacheException

cacheFile :: FilePath
cacheFile = "cardcache.dat"

apiUrl :: String
apiUrl = "https://api.hearthstonejson.com/v1/"

-- | Fetch the numeric identifier of the latest Hearthstone build
--   supported by <hearthstonejson.com>
fetchLatestBuild :: (MonadThrow m, MonadIO m) => m Int
fetchLatestBuild = do
  -- latest/ redirects us to the latest build so use that without
  -- following any redirects to get the latest build number from
  -- the url we are redirected to
  request <- parseRequest (apiUrl ++ "latest/")
  response <- httpNoBody(request{redirectCount=0})
  let code = getResponseStatusCode response
  unless (code == 302) $
    throwM (BadStatusCode code)
  let loc = case getResponseHeader "location" response of
        -- There should only be one location header, but may as well
        -- be tolerant and ignore ones after the first one
        []  -> throwM NoLocInResponse
        x:_ -> unpack x
  case List.stripPrefix apiUrl loc of
    Nothing   -> throwM (BadLocation loc)
    Just rest -> pure (read $ init rest)

-- | Fetch the latest information on cards from <hearthstonejson.com>
fetchCards :: (MonadThrow m, MonadIO m) => m (IntMap Card)
fetchCards = do
  request <- parseRequest (apiUrl ++ "latest/enUS/cards.collectible.json")
  rawData <- getResponseBody <$> httpJSON @_ @[Card] request
  pure $ List.foldl'
    (\acc card@Card{_cardDbfId=dbfId} -> IntMap.insert dbfId card acc)
    IntMap.empty
    rawData

-- | Create a new cache file by fetching data from <hearthstonejson.com>
createCache :: IO Cache
createCache = do
  b <- fetchLatestBuild
  cs <- fetchCards
  let cache = Cache b cs
  ByteString.writeFile cacheFile (encodeLazy cache)
  pure cache

-- | Load the card cache from its default file, or create the cache if
--   that file does not exist
loadCache :: IO Cache
loadCache =
    doesFileExist cacheFile >>= \case
      False -> createCache
      True  -> loadCache'
  where
    loadCache' = do
      raw <- ByteString.readFile cacheFile
      cache <- case decodeLazy raw of
        Left err    -> throwM (DeserializeFailure err)
        Right cache -> pure cache
      -- TODO: If not connected to internet, skip this.  Silently
      -- suppress failing to get data?
      latestBuild <- fetchLatestBuild
      if build cache /= latestBuild
        then createCache
        else pure cache
