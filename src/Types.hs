{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( 
    Position(..),
    BoardSize(..),

    PieceDef(..),

    FormationEntry(..),

    RuleSet(..),

    Color(..),
    Piece(..),
    GameState(..),
    
    MoveRule(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), withObject, (.:), withArray)
import Data.Vector (toList)
import Data.Text (Text)
import qualified Data.Map.Strict as Map


data Position = Pos { r :: Int, c :: Int }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Position where
  parseJSON = withArray "Position" $ \arr -> do
    [row, col] <- toList <$> traverse parseJSON arr 
    return (Pos row col)                             

data BoardSize = BoardSize { rows :: Int, cols :: Int }
  deriving (Show, Generic, FromJSON)

-- piece movement def (UPDATED)
data PieceDef = PieceDef
  { name :: Text
  , moves :: [MoveRule] 
  , symbol_white :: Char 
  , symbol_black :: Char 
  } deriving (Show, Generic, FromJSON)

-- pieces starting formation
data FormationEntry = FormationEntry
  { piece :: Text
  , position :: Position 
  } deriving (Show, Generic, FromJSON)

-- entire rule set (UPDATED)
data RuleSet = RuleSet
  { board_size :: BoardSize
  , pieces :: [PieceDef]
  , formation :: [FormationEntry]
  } deriving (Show, Generic, FromJSON)

data Color = White | Black
  deriving (Show, Eq, Ord, Generic, FromJSON)

data Piece = Piece
  { pName :: Text
  , pColor :: Color
  , pSymbol :: Char
  } deriving (Show, Eq, Ord)

data GameState = GameState
  { gsBoard  :: Map.Map Position Piece
  , gsPlayer :: Color
  } deriving (Show)

-- new rule
data MoveRule
  = Step Position  -- A single step to an adjacent square
  | Jump Position  -- A single leap to a non-adjacent square
  | Slide Position -- A repeated slide in one direction (e.g., [1,0] or [1,1])
  deriving (Show, Generic)

-- new custom parser the MoveRule AST
instance FromJSON MoveRule where
  parseJSON = withObject "MoveRule" $ \v -> do
    -- Get the "type" field from the YAML object
    ruleType <- v .: "type"
    case (ruleType :: String) of
      "Step" -> Step <$> v .: "offset"
      "Jump" -> Jump <$> v .: "offset"
      "Slide" -> Slide <$> v .: "direction"
      _ -> fail "Unknown move rule type"