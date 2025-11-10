{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( 
    Position(..),
    BoardSize(..),
    PieceDefV1(..),
    FormationEntry(..),
    RuleSetV1(..),
    Color(..),
    Piece(..),
    GameState(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), withArray)
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

-- piece movement def (v1 - simple ver)
data PieceDefV1 = PieceDefV1
  { name :: Text
  , moves :: [Position] 
  } deriving (Show, Generic, FromJSON)

-- pieces starting formation
data FormationEntry = FormationEntry
  { piece :: Text
  , position :: Position 
  } deriving (Show, Generic, FromJSON)

-- entire rule set  (v1)
data RuleSetV1 = RuleSetV1
  { board_size :: BoardSize
  , pieces :: [PieceDefV1]
  , formation :: [FormationEntry]
  } deriving (Show, Generic, FromJSON)

data Color = White | Black
  deriving (Show, Eq, Ord, Generic, FromJSON)

data Piece = Piece
  { pName :: Text
  , pColor :: Color
  } deriving (Show, Eq, Ord)

data GameState = GameState
  { gsBoard  :: Map.Map Position Piece
  , gsPlayer :: Color
  } deriving (Show)