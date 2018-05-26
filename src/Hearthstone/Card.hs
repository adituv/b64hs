{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
module Hearthstone.Card where

import           Hearthstone.Enums

import           Data.Aeson          (FromJSON (..))
import           Data.Map.Strict     (Map)
import           Data.Serialize
import           Data.Serialize.Text ()
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Lens.Micro.TH

type CardId = String

-- | Represents a Hearthstone card as fetched from the <hearthstonejson.com>
--   API
data Card = Card
  { _cardArtist             :: Text
  , _cardCardClass          :: Class
  , _cardCollectible        :: Bool
  , _cardCost               :: Maybe Int
  , _cardDbfId              :: Int
  , _cardFlavor             :: Text
  , _cardId                 :: CardId
  , _cardName               :: Text
  -- Requirements omitted because they are slightly more complicated
  -- , _cardPlayRequirements :: Map Requirement Int
  , _cardRarity             :: Maybe Rarity
  , _cardSet                :: Text
  , _cardText               :: Text
  , _cardCardType           :: Type
  , _cardMechanics          :: [Mechanic]
  , _cardAttack             :: Maybe Int
  , _cardHealth             :: Maybe Int
  , _cardReferencedTags     :: [Mechanic]
  , _cardRace               :: Race
  , _cardElite              :: Bool
  , _cardTargetingArrowText :: Text
  , _cardDurability         :: Maybe Int
  , _cardOverload           :: Int
  , _cardSpellDamage        :: Maybe Int
  , _cardHowToEarn          :: Maybe Text
  , _cardHowToEarnGolden    :: Maybe Text
  , _cardCollectionText     :: Maybe Text
  , _cardClasses            :: [Class]
  , _cardMultiClassGroup    :: MultiClassGroup
  , _cardEntourage          :: [CardId]
  , _cardFaction            :: Faction
  , _cardArmor              :: Maybe Int
  , _cardHideStats          :: Bool
  , _cardQuestReward        :: CardId
  }
  deriving (Show, Generic)

instance Serialize Card

instance FromJSON Card where
  parseJSON v = error "TODO"

makeFields ''Card

