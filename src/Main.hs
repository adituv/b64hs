{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Main where

import           Cache
import           Deck
import           Hero

import           Control.Monad          (void)
import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as ByteString
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.Serialize         as Cereal
import           System.IO

{-# INLINE (<&>) #-}
infixl 1 <&>

-- | Infix flipped fmap
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

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

getCardName :: Cache -> Int -> String
getCardName Cache{cards=cs} dbfId =
  IntMap.findWithDefault "<Unknown>" dbfId cs

pprintDeck :: Cache -> Deck -> IO ()
pprintDeck cache Deck{format=f, hero=h, cards=cs} = do
  putStrLn $ "Deck Format: " ++ show f
  let heroName = getCardName cache h
  let hClass = maybe "Unknown" show (IntMap.lookup h heroClasses)
  putStrLn $ "Deck Hero: " ++ heroName ++ " (" ++ hClass ++ ")"
  void $ IntMap.traverseWithKey
    (\k v -> do
      putStr (show v ++ "x ")
      putStrLn (getCardName cache k)
    )
    cs
