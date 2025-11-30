-- src/Main.hs (Revised for Week 3)
module Main where

import qualified Data.Yaml as Yaml
import qualified Data.Map.Strict as Map
import Types
import RuleEngine
import Validator (validateRuleSet) 
import System.Exit (exitFailure)   
import System.IO (hFlush, stdout, hSetEncoding, utf8)
import Text.Read (readMaybe)      
import Data.Char (ord, toLower, isDigit, isAlpha)


main :: IO ()
main = do
  hSetEncoding stdout utf8
  -- first run this command before doing stack run: chcp 65001

  putStrLn "--- CustomChessKell (Iteration 2) ---"
  eRuleSet <- Yaml.decodeFileEither "rules_w4.yaml"

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
      let initialBoard = buildInitialBoard boardSize (formation ruleSet) rules
      let initialState = GameState { gsBoard = initialBoard, gsPlayer = White }

      -- 5. Print initial board and start the loop
      putStrLn (renderBoard boardSize initialBoard)
      gameLoop rules boardSize initialState

-- gameloop
gameLoop :: RuleMap -> BoardSize -> GameState -> IO ()
gameLoop rules size state = do
  let player = gsPlayer state
  let board = gsBoard state

  -- --- WEEK 6: WIN CONDITION CHECK (Start of Turn) ---
  -- 1. Can the current player move?
  if not (hasLegalMoves rules size board player) then do
    -- 2. No moves left. Is it Checkmate or Stalemate?
    if isKingInCheck rules size board player then do
       putStrLn $ "\n--- CHECKMATE! ---"
       -- If current player is in checkmate, the *other* player won.
       let winner = if player == White then Black else White
       putStrLn $ show winner ++ " wins the game!"
    else do
       putStrLn $ "\n--- STALEMATE ---"
       putStrLn "The game is a Draw (No legal moves)."
    
    -- Game ends here (we do not call gameLoop again)
    return ()

  else do
    -- 3. Game continues: Prompt user for input
    putStrLn $ "--- Turn: " ++ show player ++ " ---"
    
    -- (Optional: Warn if in check)
    if isKingInCheck rules size board player then
      putStrLn "⚠️  WARNING: You are in CHECK!"
    else return ()

    putStr "Enter move (e.g., 'a2 a3'): "
    hFlush stdout 
    line <- getLine

    -- 4. Parse and Validate (Mostly unchanged, removed the old win check)
    case parseMove line of
      Left err -> do
        putStrLn $ "Error: " ++ err
        gameLoop rules size state 
      
      Right (fromPos, toPos) -> do
        let validMoves = getSafeMoves rules size board fromPos
        
        let pieceOwner = case Map.lookup fromPos board of
                           Just p -> Just (pColor p)
                           Nothing -> Nothing

        if (Just player /= pieceOwner) then do
          putStrLn $ "Error: You don't have a piece at " ++ show fromPos
          gameLoop rules size state 
          
        else if (toPos `elem` validMoves) then do
          -- Move is valid
          let piece = board Map.! fromPos
          let newBoard = Map.insert toPos piece (Map.delete fromPos board)
          
          putStrLn (renderBoard size newBoard)
          
          let opponent = if player == White then Black else White
          let newState = GameState { gsBoard = newBoard, gsPlayer = opponent }
          gameLoop rules size newState

        else do
          putStrLn $ "Error: Invalid move. " ++ show fromPos ++ " cannot move to " ++ show toPos
          gameLoop rules size state

-- Parses "a2 a3" -> (Pos, Pos)
parseMove :: String -> Either String (Position, Position)
parseMove input =
  case words input of
    [fromStr, toStr] ->
      case (parseChessPos fromStr, parseChessPos toStr) of
        (Right f, Right t) -> Right (f, t)
        (Left err, _) -> Left $ "Invalid 'from' square: " ++ err
        (_, Left err) -> Left $ "Invalid 'to' square: " ++ err
    _ -> Left "Invalid format. Use algebraic notation (e.g., 'a2 a3')."

-- Parses "e2" -> Pos {r=1, c=4}
parseChessPos :: String -> Either String Position
parseChessPos s
  | length s < 2 = Left "Coordinate too short."
  | otherwise =
      let
        colChar = toLower (head s) -- 'e'
        rowStr  = tail s           -- "2"
      in
        if not (isAlpha colChar) 
          then Left "Column must be a letter (a, b, c...)."
          else if not (all isDigit rowStr)
            then Left "Row must be a number (1, 2, 3...)."
            else
              let
                -- Convert 'a' -> 0, 'b' -> 1
                col = ord colChar - ord 'a'
                -- Convert "1" -> 0, "2" -> 1
                row = (read rowStr :: Int) - 1
              in
                Right (Pos row col)