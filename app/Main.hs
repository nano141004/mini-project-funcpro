-- src/Main.hs (Revised for Week 3)
module Main where

import qualified Data.Yaml as Yaml
import qualified Data.Map.Strict as Map
import Types
import RuleEngine
import Validator (validateRuleSet) 
import System.Exit (exitFailure)   
import System.IO (hFlush, stdout) 
import Text.Read (readMaybe)      


main :: IO ()
main = do
  putStrLn "--- CustomChessKell (Iteration 1) ---"
  eRuleSet <- Yaml.decodeFileEither "rules_w2.yaml"

  -- 2. VALIDATE the raw RuleSet
  let validatedRuleSet = case eRuleSet of
        Left err -> Left (show err)
        Right rawRuleSet -> validateRuleSet rawRuleSet

  -- 3. Check the result of the validation
  case validatedRuleSet of
    -- If validation FAILED
    Left errMsg -> do
      putStrLn "\n--- ERROR ---"
      putStrLn "Failed to load rules:"
      putStrLn errMsg
      exitFailure 

    -- If validation SUCCEEDED
    Right ruleSet -> do
      putStrLn "Successfully loaded and validated rules."
      
      -- 4. Build initial game state
      let rules = buildRuleMap (pieces ruleSet)
      let boardSize = board_size ruleSet
      let initialBoard = buildInitialBoard boardSize (formation ruleSet)
      let initialState = GameState { gsBoard = initialBoard, gsPlayer = White }

      -- 5. Print initial board and start the loop
      putStrLn (renderBoard boardSize initialBoard)
      gameLoop rules boardSize initialState

-- gameloop
gameLoop :: RuleMap -> BoardSize -> GameState -> IO ()
gameLoop rules size state = do
  let player = gsPlayer state
  let board = gsBoard state

  -- 1. Prompt user for input
  putStrLn $ "--- Turn: " ++ show player ++ " ---"
  putStr "Enter move (e.g., '1,0 2,0' for row,col row,col): "
  hFlush stdout -- Ensure the prompt appears before input
  line <- getLine

  -- 2. Try to parse and validate the move
  case parseMove line of
    Left err -> do
      putStrLn $ "Error: " ++ err
      gameLoop rules size state -- Loop again with same state
    
    Right (fromPos, toPos) -> do
      -- 3. Check if the move is legal
      let validMoves = getValidMoves rules size board fromPos
      
      -- Check that the piece being moved belongs to the current player
      let pieceOwner = case Map.lookup fromPos board of
                         Just p -> Just (pColor p)
                         Nothing -> Nothing

      if (Just player /= pieceOwner) then do
        putStrLn $ "Error: You don't have a piece at " ++ show fromPos
        gameLoop rules size state -- Loop again
        
      else if (toPos `elem` validMoves) then do
        -- 4. Move is valid! Create the new state
        let piece = board Map.! fromPos -- Get the piece
        let newBoard = Map.insert toPos piece (Map.delete fromPos board)
        
        -- Print the new board
        putStrLn (renderBoard size newBoard)

        -- 5. Check for win condition
        let opponent = if player == White then Black else White
        if not (findKing newBoard opponent) then do
          -- King is gone
          putStrLn $ "\n--- GAME OVER ---"
          putStrLn $ "Player " ++ show player ++ " wins by capturing the King!"
        
        else do
          -- 6. No win, continue to next turn
          let newState = GameState { gsBoard = newBoard, gsPlayer = opponent }
          gameLoop rules size newState

      else do
        -- 4. Move is invalid
        putStrLn $ "Error: Invalid move. " ++ show fromPos ++ " cannot move to " ++ show toPos
        gameLoop rules size state -- Loop again


-- user input
parseMove :: String -> Either String (Position, Position)
parseMove input =
  case words input of
    [fromStr, toStr] ->
      case (parsePos fromStr, parsePos toStr) of
        (Right f, Right t) -> Right (f, t)
        (Left err, _) -> Left $ "Invalid 'from' position: " ++ err
        (_, Left err) -> Left $ "Invalid 'to' position: " ++ err
    _ -> Left "Invalid input. Expected two positions (e.g., '1,0 2,0')."

-- parse pos from user input
parsePos :: String -> Either String Position
parsePos str =
  case span (/= ',') str of
    (rStr, ',':cStr) ->
      case (readMaybe rStr, readMaybe cStr) of
        (Just rInt, Just cInt) -> Right (Pos rInt cInt)
        _ -> Left $ "'" ++ str ++ "' is not in 'row,col' number format."
    _ -> Left $ "'" ++ str ++ "' is not in 'row,col' format."