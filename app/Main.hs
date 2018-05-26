{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Main where

import           Cache

import           Hearthstone.Card
import           Hearthstone.Deck

import           Control.Monad          (void)
import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as ByteString
import qualified Data.IntMap.Strict     as IntMap
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import qualified Data.Serialize         as Cereal
import qualified Data.Text              as Text
import qualified Data.Text.IO           as TIO
import           Lens.Micro
import           System.IO

main :: IO ()
main = do
  cache <- loadCache
  b64 <- ByteString.getContents
           <&> ByteString.lines
           <&> filter (\bs -> not $ "#" `ByteString.isPrefixOf` bs)
           <&> head
  deckE <- runExceptT $ do
    deckBytes <- liftEither $ Base64.decode b64
    liftEither $ Cereal.decode @Deck deckBytes
  case deckE of
    Left err -> hPutStrLn stderr err
    Right d  -> pprintDeck cache d

lookupCard :: Cache -> Int -> Maybe Card
lookupCard Cache{cards=cs} dbfId =
  IntMap.lookup dbfId cs

pprintDeck :: Cache -> Deck -> IO ()
pprintDeck cache (Deck f h cs) = do
  putStrLn $ "Deck Format: " ++ show f
  let heroCard = lookupCard cache h
  let heroName = fromMaybe "<Unknown>"
                           (heroCard ^? _Just . name)
  let hClass = heroCard ^? _Just . cardClass
  let hClassStr = maybe "<Unknown>"
                        (Text.pack . show)
                        hClass
  TIO.putStrLn $ "Deck Hero: " <> heroName <> " (" <> hClassStr <> ")"
  void $ IntMap.traverseWithKey
    (\k v -> do
      putStr (show v ++ "x ")
      let c = lookupCard cache k
      let cardName = fromMaybe "<Unknown>"
                               (c ^? _Just . name)
      TIO.putStrLn cardName
    )
    cs
