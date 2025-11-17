{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module RuleEngine where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Types
import Data.List (intersperse) 


type Board = Map.Map Position Piece
type RuleMap = Map.Map Text [MoveRule] -- updated from position to moverule


add :: Position -> Position -> Position
(Pos r1 c1) `add` (Pos r2 c2) = Pos (r1 + r2) (c1 + c2)


buildRuleMap :: [PieceDef] -> RuleMap
buildRuleMap defs = Map.fromList $ map (\d -> (name d, moves d)) defs


buildInitialBoard :: BoardSize -> [FormationEntry] -> Board
buildInitialBoard (BoardSize {..}) entries =
  let
    -- pieces for White
    whitePieces = map (createPiece White) entries

    -- reflected pieces for Black
    blackPieces = map (createPiece Black) entries

    createPiece :: Color -> FormationEntry -> (Position, Piece)
    createPiece color entry =
      let
        basePos = position entry
        pieceData = Piece { pName = piece entry, pColor = color }
      in
        -- white pieces - use the position as-is
        -- black pieces - reflect the row
        case color of
          White -> (basePos, pieceData)
          Black ->
            let
              reflectedRow = (rows - 1) - (r basePos)
              reflectedPos = Pos reflectedRow (c basePos)
            in
              (reflectedPos, pieceData)

  in
    Map.fromList (whitePieces ++ blackPieces)


isPosOnBoard :: BoardSize -> Position -> Bool
isPosOnBoard (BoardSize {..}) (Pos {..}) =
  r >= 0 && r < rows && c >= 0 && c < cols


-- update (refactor)
getValidMoves :: RuleMap -> BoardSize -> Board -> Position -> [Position]
getValidMoves rules size board fromPos =
  case Map.lookup fromPos board of
    Nothing -> [] -- No piece
    Just piece ->
      let
        -- Get the list of rules for this piece
        moveRules = Map.findWithDefault [] (pName piece) rules
        playerSign = if pColor piece == White then 1 else -1
        
        -- Apply the player sign to a move vector
        applySign (Pos dr dc) = Pos (dr * playerSign) dc
        
        -- 'concatMap' runs the interpreter for each rule and joins the lists
        evalMove rule = case rule of
          Step offset  -> evalStep (applySign offset)
          Jump offset  -> evalJump (applySign offset)
          Slide direction -> evalSlide (applySign direction)
          
      in concatMap evalMove moveRules
  where
    -- Helper: Is a square valid to land on? (On board & not friendly)
    isTargetValid toPos =
      isPosOnBoard size toPos &&
      case Map.lookup toPos board of
        Nothing -> True -- Empty is fine
        Just target -> pColor target /= pColor (board Map.! fromPos) -- Enemy is fine

    -- --- INTERPRETER LOGIC ---
    
    -- 1. Evaluates a Step
    evalStep offset =
      let toPos = fromPos `add` offset
      in if isTargetValid toPos then [toPos] else []

    -- 2. Evaluates a Jump
    --    (Identical to Step, as our v1 'isValidMove' only checks the destination)
    evalJump offset =
      let toPos = fromPos `add` offset
      in if isTargetValid toPos then [toPos] else []

    -- 3. Evaluates a Slide (The new complex logic)
    evalSlide direction =
      -- This is a recursive helper that "walks" in one direction
      let
        walk (nextPos:remaining)
          | not (isPosOnBoard size nextPos) = [] -- Hit edge of board
          | otherwise = case Map.lookup nextPos board of
              Nothing -> nextPos : walk remaining -- Empty square, add to moves and continue
              Just p -> if pColor p == pColor (board Map.! fromPos)
                        then [] -- Hit friendly piece, stop.
                        else [nextPos] -- Hit enemy piece, add capture and stop.
      in
        -- Create the "infinite" list of squares in this direction
        -- e.g., [Pos 1 0, Pos 2 0, Pos 3 0, ...]
        let path = tail [ fromPos `add` (direction `scale` n) | n <- [0..] ]
            scale (Pos dr dc) n = Pos (dr*n) (dc*n)
        in
          walk path 

renderBoard :: BoardSize -> Board -> String
renderBoard (BoardSize {..}) board =
  let
    -- Helper to get char for a single cell
    cellToChar :: Position -> Char
    cellToChar pos = case Map.lookup pos board of
      Nothing -> '.'
      Just p  -> pieceToChar p
    
    -- Helper to get char for a piece
    pieceToChar :: Piece -> Char
    pieceToChar (Piece "SimplePawn" White) = 'P'
    pieceToChar (Piece "SimplePawn" Black) = 'p'
    pieceToChar (Piece "SimpleKing" White) = 'K'
    pieceToChar (Piece "SimpleKing" Black) = 'k'

    -- --- ADDED THESE NEW CASES ---
    pieceToChar (Piece "Knight" White)     = 'N' -- 'N' for kNight
    pieceToChar (Piece "Knight" Black)     = 'n'
    pieceToChar (Piece "Rook" White)       = 'R'
    pieceToChar (Piece "Rook" Black)       = 'r'

    pieceToChar (Piece "Bishop" White)       = 'B'
    pieceToChar (Piece "Bishop" Black)       = 'b'

    pieceToChar _ = '?' -- Fallback for unknown pieces

    -- Create a list of all rows (as Strings)
    -- We print from the top row (rows-1) down to 0 for standard board layout
    rowsAsStrings = [ [cellToChar (Pos r c) | c <- [0..cols-1]] | r <- [rows-1, rows-2 .. 0] ]
    
    -- Add row numbers and spaces for readability
    -- e.g., "7 | p p p . . ."
    addRowNum r str = show r ++ " | " ++ (intersperse ' ' str)
    numberedRows = zipWith addRowNum [rows-1, rows-2 .. 0] rowsAsStrings

    -- Create the column header
    -- e.g., "  | 0 1 2 3 4 5"
    colHeader = "  | " ++ (intersperse ' ' [ (['0'..] !!) c | c <- [0..cols-1] ])
    separator = "  --" ++ replicate (cols * 2) '-'

  in
    -- Combine all lines into one string, separated by newlines
    unlines (numberedRows ++ [separator, colHeader])

-- | --- NEW FUNCTION ---
-- | Checks if a King of a given color is on the board.
-- | Returns True if king exists, False otherwise.
findKing :: Board -> Color -> Bool
findKing board kingColor =
  let
    -- The check function for the fold
    -- 'found' is the accumulator (True/False)
    -- 'piece' is the current piece
    checkKing piece found
      | found = True -- If already found, stop checking
      | pName piece == "SimpleKing" && pColor piece == kingColor = True
      | otherwise = False
  in
    -- Fold over all pieces. Start with 'False' (not found)
    -- 'checkKing' will be called on each piece
    Map.foldr checkKing False board