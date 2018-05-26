{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Deck

import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as ByteString
import qualified Data.Serialize         as Cereal
import           System.IO

{-# INLINE (<&>) #-}
infixl 1 <&>

-- | Infix flipped fmap
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

main :: IO ()
main = do
  b64 <- ByteString.getContents
           <&> ByteString.lines
           <&> filter (\bs -> not $ "#" `ByteString.isPrefixOf` bs)
           <&> head
  deckE <- runExceptT $ do
    deckBytes <- liftEither $ Base64.decode b64
    liftEither $ Cereal.decode @Deck deckBytes
  case deckE of
    Left err   -> hPutStrLn stderr err
    Right deck -> print deck
