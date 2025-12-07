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
import RuleEngine (isPosOnBoard, buildRuleMap, buildInitialBoard, isKingInCheck) 
import Data.List (group, sort)

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
  validateFormationArea boardSize formationEntries
  validateKingPresence pieceDefs formationEntries 

  validateSlideDirections pieceDefs 
  validateJumpOffsets pieceDefs 

  validateStepOffsets pieceDefs 

  validateInitialStates boardSize pieceDefs formationEntries

  validateUniqueSymbols pieceDefs
  
  -- if all checks pass, return the original RuleSet
  return rs


-- | Check 1: Ensure all pieces in 'formation' are defined in 'pieces'.
validatePieceNames :: [PieceDef] -> [FormationEntry] -> ValidationResult
validatePieceNames pieceDefs formation =
  let
    -- A Set of all valid piece names 
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


-- | Check 3: Ensure no more than one piece start on the same square.
validateFormationCollisions :: [FormationEntry] -> ValidationResult
validateFormationCollisions formation =
  let
    check entry (Right posSet) =
      let pos = position entry
      in
        if Set.member pos posSet
        then Left $ "Validation Error: Duplicate position in 'formation'. " ++
                    "Both '" ++ T.unpack (piece entry) ++ "' and another " ++
                    "piece are at " ++ show pos ++ "."
        else Right (Set.insert pos posSet)
    check _ (Left err) = Left err 
    
    initial = Right Set.empty
  in
    -- 'foldr' will run the 'check' function on each entry
    -- If 'check' ever returns a 'Left', the fold stops
    foldr check initial formation >> return ()


-- | Check 4: Ensure all pieces is on the correct side
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


-- | Check 5: Ensure the board should atleast has exactly 1 King
validateKingPresence :: [PieceDef] -> [FormationEntry] -> ValidationResult
validateKingPresence pieceDefs formation =
  let
    -- 1. Check that "King" is defined in the 'pieces' list
    kingDefined = any (\p -> name p == "King") pieceDefs
    -- 2. Check that "King" is used in the 'formation' list
    kingEntries = filter (\e -> piece e == "King") formation
    kingCount = length kingEntries
  in
    if not kingDefined then
      Left "Validation Error: A piece named 'King' must be defined in the 'pieces' list."
    else case kingCount of
      1 -> Right () -- Exactly one king
      0 -> Left "Validation Error: No 'King' found in 'formation' list. One is required."
      _ -> Left $ "Validation Error: Found " ++ show kingCount ++ " 'King' entries in 'formation'. Exactly one is required."


-- | Check 6: Slide direction checking 
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


-- | Check 7: Jump checking - should be more than 1 - if only 1, than just use step
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
    checkMove _ _ = Right () 
  in
    mapM_ checkPiece pieceDefs


-- | Check 8: Step checking - must be exactly 1 square
validateStepOffsets :: [PieceDef] -> ValidationResult
validateStepOffsets pieceDefs =
  let
    checkPiece piece = mapM_ (checkMove (name piece)) (moves piece)
    
    checkMove pieceName (Step (Pos dr dc)) =
      -- A step MUST be exactly 1 square away
      let isStepVec = max (abs dr) (abs dc) == 1
      in when (not isStepVec) $
           Left $ "Validation Error: Piece '" ++ T.unpack pieceName ++
                  "' has a Step with an invalid offset " ++ show (Pos dr dc) ++
                  ". Step offsets must be exactly 1 square."
    checkMove _ _ = Right ()
  in
    mapM_ checkPiece pieceDefs


-- | Check 9: Ensure neither side starts in Check.
validateInitialStates :: BoardSize -> [PieceDef] -> [FormationEntry] -> ValidationResult
validateInitialStates size pieces formation = do
  -- 1. Boot up the engine logic temporarily
  let rules = buildRuleMap pieces

  let board = buildInitialBoard size formation rules
  
  -- 2. Check White 
  when (isKingInCheck rules size board White) $
    Left "Validation Error: Black King starts in Check. The formation is invalid."

  -- 3. Check Black 
  when (isKingInCheck rules size board Black) $
    Left "Validation Error: Black King starts in Check. The formation is invalid."
    
  return ()


-- | Check 10: Ensure all defined symbols (white and black) are unique across all pieces.
validateUniqueSymbols :: [PieceDef] -> ValidationResult
validateUniqueSymbols pieceDefs =
  let
    -- 1. Collect ALL symbols from ALL pieces into one list
    allSymbols = concatMap (\p -> [symbol_white p, symbol_black p]) pieceDefs
    
    -- 2. Find duplicates
    -- Sort the list, group identical elements, filter groups with length > 1
    duplicates = [ x | (x:xs) <- group (sort allSymbols), not (null xs) ]
  in
    if null duplicates
    then Right ()
    else Left $ "Validation Error: Duplicate symbols detected in rules.yaml: " ++ show duplicates ++ 
                ". All symbols (White and Black) must be unique to ensure the board is readable."