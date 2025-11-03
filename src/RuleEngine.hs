{-# LANGUAGE RecordWildCards #-}
module RuleEngine where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Types


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