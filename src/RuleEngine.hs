{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module RuleEngine where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Types
import Data.List (intersperse) 


type Board = Map.Map Position Piece
type RuleMap = Map.Map Text [Position] -- e.g., "SimplePawn" -> [Pos 1 0]


add :: Position -> Position -> Position
(Pos r1 c1) `add` (Pos r2 c2) = Pos (r1 + r2) (c1 + c2)

buildRuleMap :: [PieceDefV1] -> RuleMap
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


-- -- current function handles:
-- - check whether the piece exist at given position
-- - handle movement for 
getValidMoves :: RuleMap -> BoardSize -> Board -> Position -> [Position]
getValidMoves rules size board fromPos =
  case Map.lookup fromPos board of
    Nothing -> [] -- No piece at this position
    Just piece ->
      let
        pieceName = pName piece
        moveOffsets = Map.findWithDefault [] pieceName rules
        
        -- to determine the direction
        playerSign = if pColor piece == White then 1 else -1
        
        applyOffset (Pos dr dc) = Pos (r fromPos + (dr * playerSign)) (c fromPos + dc)
        potentialPositions = map applyOffset moveOffsets

      in filter (isValidMove piece) potentialPositions
  where
    isValidMove :: Piece -> Position -> Bool
    isValidMove ourPiece toPos =
      let
        isOnBoard = isPosOnBoard size toPos
        isFriendlyOccupied = case Map.lookup toPos board of
          Nothing -> False
          Just targetPiece -> pColor targetPiece == pColor ourPiece
      in
        isOnBoard && (not isFriendlyOccupied)  

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