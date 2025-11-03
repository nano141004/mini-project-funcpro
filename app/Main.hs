module Main where

import qualified Data.Yaml as Yaml
import qualified Data.Map.Strict as Map
import Types
import RuleEngine
import Validator (validateRuleSet) 
import System.Exit (exitFailure)   

main :: IO ()
main = do
  putStrLn "--- CustomChessKell Prog 2  ---"

  -- 1) load Rules - YAML format
  eRuleSet <- Yaml.decodeFileEither "rules_w2.yaml"

  -- 2) validate rules defined
  let validatedRuleSet = case eRuleSet of
        Left err -> Left (show err) 
        Right rawRuleSet -> validateRuleSet rawRuleSet

  case validatedRuleSet of
    -- rule validation failed
    Left errMsg -> do
      putStrLn "\n--- ERROR ---"
      putStrLn "Failed to load rules:"
      putStrLn errMsg
      exitFailure 

    -- rule validation succeed
    Right ruleSet -> do
      putStrLn "Successfully loaded and validated rules."

      -- build game state
      let rules = buildRuleMap (pieces ruleSet)
      let boardSize = board_size ruleSet
      let board = buildInitialBoard boardSize (formation ruleSet)

      putStrLn "Built initial board and rule map."
      
      -- test simple cases
      putStrLn "\n--- TESTING ENGINE ---"
      runTests rules boardSize board
      putStrLn "\n--- TEST COMPLETE ---"


runTests :: RuleMap -> BoardSize -> Board -> IO ()
runTests rules boardSize board = do
  -- Test for 5 x 5 board
  -- Test 1: White Pawn
  let whitePawnPos = Pos 1 1
  let whitePawnMoves = getValidMoves rules boardSize board whitePawnPos
  putStrLn $ "Moves for White Pawn at (1,1): " ++ show whitePawnMoves

  -- Test 2: White King
  let whiteKingPos = Pos 0 2
  let whiteKingMoves = getValidMoves rules boardSize board whiteKingPos
  putStrLn $ "Moves for White King at (0,2): " ++ show whiteKingMoves
  
  -- Test 3: Black Pawn
  let blackPawnPos = Pos 3 1 
  let blackPawnMoves = getValidMoves rules boardSize board blackPawnPos
  putStrLn $ "Moves for Black Pawn at (3,1): " ++ show blackPawnMoves

    -- Test 4: Black King
  let blackPawnPos = Pos 4 2 
  let blackPawnMoves = getValidMoves rules boardSize board blackPawnPos
  putStrLn $ "Moves for Black King at (4,2): " ++ show blackPawnMoves