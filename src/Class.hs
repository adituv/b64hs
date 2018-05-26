{-# LANGUAGE LambdaCase #-}
module Class(Class(..)) where

import           Data.Serialize
import           Data.VarInt

data Class = DeathKnight
           | Druid
           | Hunter
           | Mage
           | Paladin
           | Priest
           | Rogue
           | Shaman
           | Warlock
           | Warrior
           | Dream
           | Neutral
           deriving Show

getClass :: Get Class
getClass = getVarInt >>= \case
  1 -> pure DeathKnight
  2 -> pure Druid
  3 -> pure Hunter
  4 -> pure Mage
  5 -> pure Paladin
  6 -> pure Priest
  7 -> pure Rogue
  8 -> pure Shaman
  9 -> pure Warlock
  10 -> pure Warrior
  11 -> pure Dream
  12 -> pure Neutral

putClass :: Class -> Put
putClass DeathKnight = putVarInt 1
putClass Druid       = putVarInt 2
putClass Hunter      = putVarInt 3
putClass Mage        = putVarInt 4
putClass Paladin     = putVarInt 5
putClass Priest      = putVarInt 6
putClass Rogue       = putVarInt 7
putClass Shaman      = putVarInt 8
putClass Warlock     = putVarInt 9
putClass Warrior     = putVarInt 10
putClass Dream       = putVarInt 11
putClass Neutral     = putVarInt 12
