{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Validator
  ( validateRuleSet
  ) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing)
import Control.Monad (when)
import Types
import RuleEngine (isPosOnBoard) 

type ValidationResult = Either String ()


validateRuleSet :: RuleSet -> Either String RuleSet
validateRuleSet rs = do
  let pieceDefs = pieces rs
  let formationEntries = formation rs
  let boardSize = board_size rs

  -- current covered cases
  validatePieceNames pieceDefs formationEntries
  validateFormationPositions boardSize formationEntries
  validateFormationCollisions formationEntries
  -- validateStepMoves pieceDefs  -- removed for now
  validateFormationArea boardSize formationEntries
  validateKingPresence pieceDefs formationEntries -- adjustment

  validateSlideDirections pieceDefs -- <-- ADDED NEW cases
  validateJumpOffsets pieceDefs -- <-- ADDED NEW cases
  
  -- if all checks pass, return the original RuleSet
  return rs

-- | Check 1: Ensure all pieces in 'formation' are defined in 'pieces'.
validatePieceNames :: [PieceDef] -> [FormationEntry] -> ValidationResult
validatePieceNames pieceDefs formation =
  let
    -- A Set of all valid piece names (e.g., {"SimplePawn", "SimpleKing"})
    definedNames = Set.fromList $ map name pieceDefs
    
    -- Check each entry in the formation
    check entry =
      let pieceName = piece entry
      in
        when (Set.notMember pieceName definedNames) $
          Left $ "Validation Error: Piece name '" ++ T.unpack pieceName ++ 
                 "' in 'formation' is not defined in the 'pieces' list."
  in
    -- 'mapM_' runs 'check' on every entry and stops at the first 'Left'
    mapM_ check formation

-- | Check 2: Ensure all pieces in 'formation' start on the board.
validateFormationPositions :: BoardSize -> [FormationEntry] -> ValidationResult
validateFormationPositions boardSize formation =
  let
    check entry =
      let pos = position entry
      in
        when (not (isPosOnBoard boardSize pos)) $
          Left $ "Validation Error: Piece '" ++ T.unpack (piece entry) ++
                 "' starts at " ++ show pos ++ ", which is off the board."
  in
    mapM_ check formation

-- | Check 3: Ensure no two pieces start on the same square.
validateFormationCollisions :: [FormationEntry] -> ValidationResult
validateFormationCollisions formation =
  let
    -- We fold the list into a Set of positions, checking for duplicates
    -- as we go.
    check entry (Right posSet) =
      let pos = position entry
      in
        if Set.member pos posSet
        then Left $ "Validation Error: Duplicate position in 'formation'. " ++
                    "Both '" ++ T.unpack (piece entry) ++ "' and another " ++
                    "piece are at " ++ show pos ++ "."
        else Right (Set.insert pos posSet)
    check _ (Left err) = Left err -- Propagate any previous error
    
    -- Start with an empty Set. The 'foldr' will build it up.
    initial = Right Set.empty
  in
    -- 'foldr' will run the 'check' function on each entry
    -- If 'check' ever returns a 'Left', the fold stops
    foldr check initial formation >> return () -- '>> return ()' discards the Set and returns 'Right ()' on success

-- Check 4
-- validateStepMoves :: [PieceDef] -> ValidationResult
-- validateStepMoves pieceDefs =
--   let
--     -- Check a single piece definition
--     checkPiece piece =
--       let pieceName = name piece
          
--           -- Check a single move offset (e.g., [2, 0])
--           checkMove moveOffset =
--             let 
--                 (Pos dr dc) = moveOffset
--                 -- A move is a "step" if its largest component (row or col) is 1
--                 isStepMove = max (abs dr) (abs dc) == 1
--             in
--               when (not isStepMove) $
--                 Left $ "Validation Error: Piece '" ++ T.unpack pieceName ++
--                        "' has an invalid move " ++ show moveOffset ++
--                        ". Iteration 1 only supports single-step moves (max 1 square in any direction)."
--       in
--         -- Run checkMove on all of the piece's moves
--         mapM_ checkMove (moves piece)
--   in
--     -- Run checkPiece on all defined pieces
--     mapM_ checkPiece pieceDefs

-- Check 5
validateFormationArea :: BoardSize -> [FormationEntry] -> ValidationResult
validateFormationArea boardSize formation =
  let
    -- Calculate the "border". For 8 rows, `8 `div` 2` is 4.
    -- So, allowed rows are 0, 1, 2, 3 (which is < 4).
    maxAllowedRow = (rows boardSize) `div` 2
    
    check entry =
      let 
        pos = position entry
        pieceName = piece entry
      in
        when (r pos >= maxAllowedRow) $
          Left $ "Validation Error: Piece '" ++ T.unpack pieceName ++
                 "' at " ++ show pos ++ " is on the wrong side of the board. " ++
                 "All 'formation' pieces must be in a row < " ++ show maxAllowedRow ++ "."
  in
    mapM_ check formation

-- adjustment
validateKingPresence :: [PieceDef] -> [FormationEntry] -> ValidationResult
validateKingPresence pieceDefs formation =
  let
    -- 1. Check that "SimpleKing" is defined in the 'pieces' list
    kingDefined = any (\p -> name p == "SimpleKing") pieceDefs
    -- 2. Check that "SimpleKing" is used in the 'formation' list
    kingEntries = filter (\e -> piece e == "SimpleKing") formation
    kingCount = length kingEntries
  in
    if not kingDefined then
      Left "Validation Error: A piece named 'SimpleKing' must be defined in the 'pieces' list."
    else case kingCount of
      1 -> Right () -- Exactly one king, this is correct.
      0 -> Left "Validation Error: No 'SimpleKing' found in 'formation' list. One is required."
      _ -> Left $ "Validation Error: Found " ++ show kingCount ++ " 'SimpleKing' entries in 'formation'. Exactly one is required."

validateSlideDirections :: [PieceDef] -> ValidationResult
validateSlideDirections pieceDefs =
  let
    checkPiece piece = mapM_ (checkMove (name piece)) (moves piece)
    
    checkMove pieceName (Slide (Pos dr dc)) =
      let isSlideVec = max (abs dr) (abs dc) == 1
      in when (not isSlideVec) $
           Left $ "Validation Error: Piece '" ++ T.unpack pieceName ++
                  "' has a Slide with an invalid direction " ++ show (Pos dr dc) ++
                  ". Slide directions must be 1-step vectors."
    checkMove _ _ = Right () -- Don't check Step or Jump
  in
    mapM_ checkPiece pieceDefs

-- --- NEW CHECK 7 ---
validateJumpOffsets :: [PieceDef] -> ValidationResult
validateJumpOffsets pieceDefs =
  let
    checkPiece piece = mapM_ (checkMove (name piece)) (moves piece)
    
    checkMove pieceName (Jump (Pos dr dc)) =
      -- A jump MUST be more than 1 square away
      let isJumpVec = max (abs dr) (abs dc) > 1
      in when (not isJumpVec) $
           Left $ "Validation Error: Piece '" ++ T.unpack pieceName ++
                  "' has a Jump with an invalid offset " ++ show (Pos dr dc) ++
                  ". Jumps must be more than 1 square."
    checkMove _ _ = Right () -- Don't check Step or Slide
  in
    mapM_ checkPiece pieceDefs