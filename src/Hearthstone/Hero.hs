{-# LANGUAGE OverloadedLists #-}
module Hearthstone.Hero(heroClasses) where

import           Hearthstone.Class

import           Data.IntMap.Strict (IntMap)
import           Data.Serialize
import           Data.VarInt

heroClasses :: IntMap Class
heroClasses =
  [ (271, Druid)     -- Malfurion
  , (50484, Druid)   -- Lunara
  , (31, Hunter)     -- Rexxar
  , (2826, Hunter)   -- Alleria
  , (637, Mage)      -- Jaina
  , (2829, Mage)     -- Medivh
  , (39117, Mage)    -- Khadgar
  , (671, Paladin)   -- Uther
  , (2827, Paladin)  -- Liadrin
  , (46116, Paladin) -- Arthas
  , (813, Priest)    -- Anduin
  , (41887, Priest)  -- Tyrande
  , (930, Rogue)     -- Valeera
  , (40195, Rogue)   -- Maiev
  , (1066, Shaman)   -- Thrall
  , (40183, Shaman)  -- Morgl
  , (893, Warlock)   -- Gul'Dan
  , (47817, Warlock) -- Nemsy
  , (7, Warrior)     -- Garrosh
  , (2828, Warrior)  -- Magni
  ]
