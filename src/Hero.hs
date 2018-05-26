module Hero(Hero(..), heroClass) where

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
          deriving Show

instance Serialize Hero where
  get = getHero
  put = putHero

getHero :: Get Hero
getHero = do
  x <- getVarInt
  case x of
    7    -> pure Garrosh
    31   -> pure Rexxar
    274  -> pure Malfurion
    637  -> pure Jaina
    671  -> pure Uther
    813  -> pure Anduin
    893  -> pure Gul'Dan
    930  -> pure Valeera
    1066 -> pure Thrall
    _    -> pure (Unknown x)

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

heroClass :: Hero -> String
heroClass (Unknown _) = "Unknown"
heroClass Garrosh     = "Warrior"
heroClass Rexxar      = "Hunter"
heroClass Malfurion   = "Druid"
heroClass Jaina       = "Mage"
heroClass Uther       = "Paladin"
heroClass Anduin      = "Priest"
heroClass Gul'Dan     = "Warlock"
heroClass Valeera     = "Rogue"
heroClass Thrall      = "Shaman"
