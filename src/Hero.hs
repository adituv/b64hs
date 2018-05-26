module Hero(Hero(..), heroClass) where

import           Class

import           Data.Serialize
import           Data.VarInt

data Hero = Unknown Int
          | Malfurion
          | Rexxar
          | Jaina
          | Uther
          | Anduin
          | Valeera
          | Thrall
          | Gul'Dan
          | Garrosh
          | Lunara
          | Alleria
          | Medivh
          | Khadgar
          | Liadrin
          | Arthas
          | Tyrande
          | Maiev
          | Morgl
          | Nemsy
          | Magni
          deriving Show

instance Serialize Hero where
  get = getHero
  put = putHero

getHero :: Get Hero
getHero = do
  x <- getVarInt
  case x of
    7     -> pure Garrosh
    31    -> pure Rexxar
    274   -> pure Malfurion
    637   -> pure Jaina
    671   -> pure Uther
    813   -> pure Anduin
    893   -> pure Gul'Dan
    930   -> pure Valeera
    1066  -> pure Thrall
    2827  -> pure Liadrin
    40183 -> pure Morgl
    40195 -> pure Maiev
    50484 -> pure Lunara
    _     -> pure (Unknown x)

putHero :: Hero -> Put
putHero (Unknown x) = putVarInt x
putHero Garrosh     = putVarInt 7
putHero Rexxar      = putVarInt 31
putHero Malfurion   = putVarInt 271
putHero Jaina       = putVarInt 637
putHero Uther       = putVarInt 671
putHero Anduin      = putVarInt 813
putHero Gul'Dan     = putVarInt 893
putHero Valeera     = putVarInt 930
putHero Thrall      = putVarInt 1066
putHero Liadrin     = putVarInt 2827
putHero Morgl       = putVarInt 40183
putHero Maiev       = putVarInt 40195
putHero Lunara      = putVarInt 50484
putHero other       = putVarInt 0

heroClass :: Hero -> Maybe Class
heroClass (Unknown _) = Nothing
heroClass Malfurion   = Just Druid
heroClass Rexxar      = Just Hunter
heroClass Jaina       = Just Mage
heroClass Uther       = Just Paladin
heroClass Anduin      = Just Priest
heroClass Valeera     = Just Rogue
heroClass Thrall      = Just Shaman
heroClass Gul'Dan     = Just Warlock
heroClass Garrosh     = Just Warrior
heroClass Lunara      = Just Druid
heroClass Alleria     = Just Hunter
heroClass Medivh      = Just Mage
heroClass Khadgar     = Just Mage
heroClass Liadrin     = Just Paladin
heroClass Arthas      = Just Paladin
heroClass Tyrande     = Just Priest
heroClass Maiev       = Just Rogue
heroClass Morgl       = Just Shaman
heroClass Nemsy       = Just Warlock
heroClass Magni       = Just Warrior
