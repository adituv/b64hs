{-# LANGUAGE TupleSections #-}
module Hearthstone.Deck(Deck(..), Format(..)) where

import           Hearthstone.Hero

import           Control.Monad      (replicateM, unless, void)
import           Data.Foldable      (traverse_)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Serialize
import           Data.VarInt

data Deck = Deck
  { format :: Format
  , hero   :: Int
  , cards  :: IntMap Int
  } deriving Show

instance Serialize Deck where
  get = getDeck
  put = putDeck

getDeck :: Get Deck
getDeck = do
  res <- getWord8 -- Reserved byte
  unless (res == 0) $
    fail "Reserved byte not 0"
  ver <- getVarInt -- Version
  unless (ver == 1) $
    fail ("Unknown deck version: " ++ show ver)
  fmt <- getFormat

  lenHeroes <- getVarInt
  unless (lenHeroes == 1) $
    fail ("Invalid hero count: " ++ show lenHeroes)
  hero <- getVarInt
  cards1 <- getVIArray
  cards2 <- getVIArray
  cardsN <- getKVArray
  let cards = combineCardsArrays cards1 cards2 cardsN

  pure Deck
    { format = fmt
    , hero = hero
    , cards = cards
    }

putDeck :: Deck -> Put
putDeck Deck{format=f, hero=h, cards=cs} = do
  putWord8 0  -- Reserved byte
  putVarInt 1 -- Version
  putFormat f

  putVarInt 1 -- Length of heroes array
  putVarInt h
  let (cards1, cards2, cardsN) = splitCardData cs
  putVIArray cards1
  putVIArray cards2
  putKVArray cardsN

getVIArray :: Get [Int]
getVIArray = do
  len <- getVarInt
  replicateM len getVarInt

putVIArray :: [Int] -> Put
putVIArray xs = do
  putVarInt (length xs)
  traverse_ putVarInt xs

getKVArray :: Get (IntMap Int)
getKVArray = do
  len <- getVarInt
  unless (even len) $
    fail ("Length of kv array is odd" ++ show len)
  kvPairs <- replicateM (len `div` 2)
               ((,) <$> getVarInt <*> getVarInt)
  pure $ IntMap.fromList kvPairs

putKVArray :: IntMap Int -> Put
putKVArray kvs = do
  putVarInt $ 2 * IntMap.size kvs
  void $ IntMap.traverseWithKey
           (\k v -> putVarInt k >> putVarInt v) kvs

combineCardsArrays :: [Int] -> [Int] -> IntMap Int -> IntMap Int
combineCardsArrays singles doubles multiples =
  IntMap.unions [ IntMap.fromList $ fmap (,1) singles
                , IntMap.fromList $ fmap (,2) doubles
                , multiples
                ]

splitCardData :: IntMap Int -> ([Int], [Int], IntMap Int)
splitCardData cards =
  let
    (singleMap, rest) = IntMap.partition (==1) cards
    (doubleMap, mults) = IntMap.partition (==2) rest
  in
    (IntMap.keys singleMap, IntMap.keys doubleMap, mults)

data Format = Standard | Wild deriving Show

instance Serialize Format where
  get = getFormat
  put = putFormat

getFormat :: Get Format
getFormat = do
  x <- getVarInt
  case x of
    1 -> pure Wild
    2 -> pure Standard
    _ -> fail ("Invalid Format: " ++ show x)

putFormat :: Format -> Put
putFormat Wild     = putVarInt 1
putFormat Standard = putVarInt 2

