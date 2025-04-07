{-# LANGUAGE MonoLocalBinds #-}

module Todoist.Data.Common where

import Data.Aeson (
  FromJSON (parseJSON),
  GFromJSON,
  GToJSON',
  ToJSON (toJSON),
  Value,
  Zero,
 )
import Data.Aeson.Types (
  Options (constructorTagModifier, fieldLabelModifier),
  Parser,
  camelTo2,
  defaultOptions,
  genericParseJSON,
  genericToJSON,
 )
import Data.Text (Text)
import GHC.Generics (Generic (Rep))

todoistToJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => a -> Value
todoistToJSON =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_'
      , constructorTagModifier = camelTo2 '_'
      }

todoistParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
todoistParseJSON =
  genericParseJSON
    defaultOptions
      { fieldLabelModifier = camelTo2 '_'
      , constructorTagModifier = camelTo2 '_'
      }

data Paginated a = Paginated
  { results :: a
  , nextCursor :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (Paginated a) where
  parseJSON = todoistParseJSON

instance (ToJSON a) => ToJSON (Paginated a) where
  toJSON = todoistToJSON

data Color
  = BerryRed
  | Red
  | Orange
  | Yellow
  | OliveGreen
  | LimeGreen
  | Green
  | MintGreen
  | Teal
  | SkyBlue
  | LightBlue
  | Blue
  | Grape
  | Violet
  | Lavender
  | Magenta
  | Salmon
  | Charcoal
  | Grey
  | Taupe
  deriving (Show, Eq, Generic)

instance FromJSON Color where
  parseJSON = todoistParseJSON

instance ToJSON Color where
  toJSON = todoistToJSON

data ViewStyle = List | Board
  deriving (Show, Eq, Generic)

instance FromJSON ViewStyle where
  parseJSON = todoistParseJSON

instance ToJSON ViewStyle where
  toJSON = todoistToJSON
