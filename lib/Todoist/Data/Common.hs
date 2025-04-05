module Todoist.Data.Common where

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
  deriving (Show, Eq)

data ViewStyle = List | Board
  deriving (Show, Eq)
