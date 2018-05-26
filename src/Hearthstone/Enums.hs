{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module Hearthstone.Enums where

import           Data.Aeson
import           Data.Serialize
import           Data.VarInt
import           GHC.Generics   (Generic)

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
           deriving (Show, Generic)

instance Serialize Class

instance FromJSON Class where
  parseJSON v = parseJSON v >>= \case
    "DEATHKNIGHT" -> pure DeathKnight
    "DRUID" -> pure Druid
    "HUNTER" -> pure Hunter
    "MAGE" -> pure Mage
    "PALADIN" -> pure Paladin
    "PRIEST" -> pure Priest
    "ROGUE" -> pure Rogue
    "SHAMAN" -> pure Shaman
    "WARLOCK" -> pure Warlock
    "WARRIOR" -> pure Warrior
    "DREAM" -> pure Dream
    "NEUTRAL" -> pure Neutral
    other -> fail $ "Invalid class" ++ other

data Rarity = Basic
            | Common
            | Rare
            | Epic
            | Legendary
            deriving (Show, Generic)

instance Serialize Rarity

instance FromJSON Rarity where
  parseJSON v = parseJSON v >>= \case
    "FREE" -> pure Basic
    "COMMON" -> pure Common
    "RARE" -> pure Rare
    "EPIC" -> pure Epic
    "LEGENDARY" -> pure Legendary
    other -> fail $ "Invalid rarity: " ++ other

data Set = Core
         | Expert1
         | HOF
         | Missions
         | Naxx
         | GVG
         | BRM
         | TGT
         | Credits
         | HeroSkins
         | TB
         | LOE
         | OG
         | Kara
         | Gangs
         | Ungoro
         | IceCrown
         | Lootapalooza
         | Gilneas
         deriving (Show, Generic)

instance Serialize Set

instance FromJSON Set where
  parseJSON v = parseJSON v >>= \case
    "CORE" -> pure Core
    "EXPERT1" -> pure Expert1
    "HOF" -> pure HOF
    "MISSIONS" -> pure Missions
    "NAXX" -> pure Naxx
    "GVG" -> pure GVG
    "BRM" -> pure BRM
    "TGT" -> pure TGT
    "CREDITS" -> pure Credits
    "HERO_SKINS" -> pure HeroSkins
    "TB" -> pure TB
    "LOE" -> pure LOE
    "OG" -> pure OG
    "KARA" -> pure Kara
    "GANGS" -> pure Gangs
    "UNGORO" -> pure Ungoro
    "ICECROWN" -> pure IceCrown
    "LOOTAPALOOZA" -> pure Lootapalooza
    "GILNEAS" -> pure Gilneas
    other -> fail $ "Invalid set: " ++ other

data Race = Beast
          | Dragon
          | Demon
          | Pirate
          | Totem
          | Murloc
          | Elemental
          | Mechanical
          | All
  deriving (Show, Generic)

instance Serialize Race

instance FromJSON Race where
  parseJSON v = parseJSON v >>= \case
    "BEAST" -> pure Beast
    "DRAGON" -> pure Dragon
    "DEMON" -> pure Demon
    "PIRATE" -> pure Pirate
    "TOTEM" -> pure Totem
    "MURLOC" -> pure Murloc
    "ELEMENTAL" -> pure Elemental
    "MECHANICAL" -> pure Mechanical
    "ALL" -> pure All
    other -> fail $ "Invalid race: " ++ other

data Faction = Alliance
             | Horde
  deriving (Show, Generic)

instance Serialize Faction

instance FromJSON Faction where
  parseJSON v = parseJSON v >>= \case
    "ALLIANCE" -> pure Alliance
    "HORDE" -> pure Horde
    other -> fail $ "Inavlid faction: " ++ other

data Mechanic = Adapt
              | AffectedBySpellPower
              | AIMustPlay
              | AppearFunctionallyDead
              | Aura
              | Autoattack
              | Battlecry
              | CantAttack
              | CantBeFatigued
              | CantBeSilenced
              | CantBeTargetedBySpells
              | CantBeTargetedByHeroPowers
              | Charge
              | ChooseOne
              | CollectionManagerFilterManaEven
              | CollectionManagerFilterManaOdd
              | Combo
              | Counter
              | DeathKnightMechanic
              | Deathrattle
              | Discover
              | DivineShield
              | DungeonPassiveBuff
              | Echo
              | EnchantmentInvisible
              | Enraged
              | EvilGlow
              | Forgetful
              | Freeze
              | GrimyGoons
              | HeroPowerDamage
              | Immune
              | ImmuneToSpellPower
              | Inspire
              | InvisibleDeathrattle
              | JadeGolem
              | JadeLotus
              | Kabal
              | Lifesteal
              | Morph
              | MultiplyBuffValue
              | Overload
              | Poisonous
              | Quest
              | ReceivesDoubleSpellDamageBonus
              | Recruit
              | Ritual
              | Rush
              | Secret
              | Silence
              | SpellPower
              | StartOfGame
              | Stealth
              | TagOneTurnEffect
              | Taunt
              | TopDeck
              | TriggerVisual
              | Untouchable
              | Windfury
  deriving (Show, Generic)

instance Serialize Mechanic

instance FromJSON Mechanic where
  parseJSON v = parseJSON v >>= \case
    "ADAPT" -> pure Adapt
    "AFFECTED_BY_SPELL_POWER" -> pure AffectedBySpellPower
    "AI_MUST_PLAY" -> pure AIMustPlay
    "APPEAR_FUNCTIONALLY_DEAD" -> pure AppearFunctionallyDead
    "AURA" -> pure Aura
    "AUTOATTACK" -> pure Autoattack
    "BATTLECRY" -> pure Battlecry
    "CANT_ATTACK" -> pure CantAttack
    "CANT_BE_FATIGUED" -> pure CantBeFatigued
    "CANT_BE_SILENCED" -> pure CantBeSilenced
    "CANT_BE_TARGETED_BY_SPELLS" -> pure CantBeTargetedBySpells
    "CANT_BE_TARGETED_BY_HERO_POWERS" -> pure CantBeTargetedByHeroPowers
    "CHARGE" -> pure Charge
    "CHOOSE_ONE" -> pure ChooseOne
    "COLLECTION_MANAGER_FILTER_MANA_EVEN" -> pure CollectionManagerFilterManaEven
    "COLLECTION_MANAGER_FILTER_MANA_ODD" -> pure CollectionManagerFilterManaOdd
    "COMBO" -> pure Combo
    "COUNTER" -> pure Counter
    "DEATH_KNIGHT" -> pure DeathKnightMechanic
    "DEATHRATTLE" -> pure Deathrattle
    "DISCOVER" -> pure Discover
    "DIVINE_SHIELD" -> pure DivineShield
    "DUNGEON_PASSIVE_BUFF" -> pure DungeonPassiveBuff
    "ECHO" -> pure Echo
    "ENCHANTMENT_INVISIBLE" -> pure EnchantmentInvisible
    "ENRAGED" -> pure Enraged
    "EVIL_GLOW" -> pure EvilGlow
    "FORGETFUL" -> pure Forgetful
    "FREEZE" -> pure Freeze
    "GRIMY_GOONS" -> pure GrimyGoons
    "HERO_POWER_DAMAGE" -> pure HeroPowerDamage
    "IMMUNE" -> pure Immune
    "IMMUNE_TO_SPELL_POWER" -> pure ImmuneToSpellPower
    "INSPIRE" -> pure Inspire
    "INVISIBLE_DEATHRATTLE" -> pure InvisibleDeathrattle
    "JADE_GOLEM" -> pure JadeGolem
    "JADE_LOTUS" -> pure JadeLotus
    "KABAL" -> pure Kabal
    "LIFESTEAL" -> pure Lifesteal
    "MORPH" -> pure Morph
    "MULTIPLY_BUFF_VALUE" -> pure MultiplyBuffValue
    "OVERLOAD" -> pure Overload
    "POISONOUS" -> pure Poisonous
    "QUEST" -> pure Quest
    "RECEIVES_DOUBLE_SPELL_DAMAGE_BONUS" -> pure ReceivesDoubleSpellDamageBonus
    "RECRUIT" -> pure Recruit
    "RITUAL" -> pure Ritual
    "RUSH" -> pure Rush
    "SECRET" -> pure Secret
    "SILENCE" -> pure Silence
    "SPELL_POWER" -> pure SpellPower
    "START_OF_GAME" -> pure StartOfGame
    "STEALTH" -> pure Stealth
    "TAG_ONE_TURN_EFFECT" -> pure TagOneTurnEffect
    "TAUNT" -> pure Taunt
    "TOP_DECK" -> pure TopDeck
    "TRIGGER_VISUAL" -> pure TriggerVisual
    "UNTOUCHABLE" -> pure Untouchable
    "WINDFURY" -> pure Windfury
    other -> fail $ "Invalid mechanic: " ++ other

data Type = Hero
          | Minion
          | Spell
          | Enchantment
          | Weapon
          | HeroPower
  deriving (Show, Generic)

instance Serialize Type

instance FromJSON Type where
  parseJSON v = parseJSON v >>= \case
    "HERO" -> pure Hero
    "MINION" -> pure Minion
    "SPELL" -> pure Spell
    "ENCHANTMENT" -> pure Enchantment
    "WEAPON" -> pure Weapon
    "HERO_POWER" -> pure HeroPower
    other -> fail $ "Invalid card type: " ++ other

data MultiClassGroup = GrimyGoonsMCG
                     | JadeLotusMCG
                     | KabalMCG
  deriving (Show, Generic)

instance Serialize MultiClassGroup

instance FromJSON MultiClassGroup where
  parseJSON v = parseJSON v >>= \case
    "GRIMY_GOONS" -> pure GrimyGoonsMCG
    "JADE_LOTUS" -> pure JadeLotusMCG
    "KABAL"    -> pure KabalMCG
    other -> fail $ "Invalid MCG: " ++ other
