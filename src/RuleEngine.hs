{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module RuleEngine where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Types
import Data.List (intersperse) 


type Board = Map.Map Position Piece

data PieceRules = PieceRules
  { rMoves :: [MoveRule]
  , rSymWhite :: Char
  , rSymBlack :: Char
  }
type RuleMap = Map.Map Text PieceRules


add :: Position -> Position -> Position
(Pos r1 c1) `add` (Pos r2 c2) = Pos (r1 + r2) (c1 + c2)


buildRuleMap :: [PieceDef] -> RuleMap
buildRuleMap defs = Map.fromList $ map toEntry defs
  where
    toEntry d = (name d, PieceRules (moves d) (symbol_white d) (symbol_black d))


buildInitialBoard :: BoardSize -> [FormationEntry] -> RuleMap -> Board
buildInitialBoard (BoardSize {..}) entries rules =
  let
    -- pieces for White
    whitePieces = map (createPiece White) entries

    -- reflected pieces for Black
    blackPieces = map (createPiece Black) entries

    createPiece :: Color -> FormationEntry -> (Position, Piece)
    createPiece color entry =
      let
        basePos = position entry
        pText   = piece entry
        
        -- Look up rules (using defaults if missing)
        pRules  = Map.findWithDefault (PieceRules [] '?' '?') pText rules
        
        -- Select the correct symbol based on color
        sym = case color of
          White -> rSymWhite pRules
          Black -> rSymBlack pRules

        pieceData = Piece 
          { pName = pText
          , pColor = color
          , pSymbol = sym -- Assign the specific symbol
          }
      in
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


-- valid moves mean every move from the piece that is within the board, the movement to the target pos isnt blocked, and there is no "friend" piece on the target pos
getValidMoves :: RuleMap -> BoardSize -> Board -> Position -> [Position]
getValidMoves rules size board fromPos =
  case Map.lookup fromPos board of
    Nothing -> [] -- No piece
    Just piece ->
      let
        -- Get the list of rules for this piece
        moveRules = case Map.lookup (pName piece) rules of
          Nothing -> [] -- If piece not found, no moves
          Just pr -> rMoves pr -- Extract the moves from the struct
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
    -- Is a square valid to land on? (On board & not friendly)
    isTargetValid toPos =
      isPosOnBoard size toPos &&
      case Map.lookup toPos board of
        Nothing -> True 
        Just target -> pColor target /= pColor (board Map.! fromPos) -- Enemy is fine

    -- --- INTERPRETER LOGIC ---
    
    -- 1. Evaluates a Step
    evalStep offset =
      let toPos = fromPos `add` offset
      in if isTargetValid toPos then [toPos] else []

    -- 2. Evaluates a Jump
    evalJump offset =
      let toPos = fromPos `add` offset
      in if isTargetValid toPos then [toPos] else []

    -- 3. Evaluates a Slide 
    evalSlide direction =
      -- recursively adding one 
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
    cellToChar :: Position -> Char
    cellToChar pos = case Map.lookup pos board of
      Nothing -> if (r pos + c pos) `mod` 2 == 0 then ' ' else '.' 
      Just p  -> pSymbol p

    -- Create rows (Top to Bottom)
    rowsAsStrings = [ [cellToChar (Pos r c) | c <- [0..cols-1]] | r <- [rows-1, rows-2 .. 0] ]
    
    -- Add Row Numbers (1-based labels)
    -- display '1' for index 0
    addRowNum r str = show (r + 1) ++ " | " ++ (intersperse ' ' str)
    numberedRows = zipWith addRowNum [rows-1, rows-2 .. 0] rowsAsStrings

    -- Create Column Header (a, b, c...)
    colLabels = intersperse ' ' (take cols ['a'..])
    colHeader = "    " ++ colLabels
    separator = "   " ++ replicate (cols * 2) '-'

  in
    unlines (numberedRows ++ [separator, colHeader])


-- Checks if a King of a given color is on the board.
-- Returns True if king exists, False otherwise.
findKing :: Board -> Color -> Bool
findKing board kingColor =
  let
    checkKing piece found
      | found = True -- If already found, stop checking
      | pName piece == "King" && pColor piece == kingColor = True
      | otherwise = False
  in
    Map.foldr checkKing False board


findKingPos :: Board -> Color -> Maybe Position
findKingPos board color = 
  let match (pos, piece) = pName piece == "King" && pColor piece == color
      results = filter match (Map.toList board)
  in case results of
       [] -> Nothing
       ((pos, _):_) -> Just pos


isKingInCheck :: RuleMap -> BoardSize -> Board -> Color -> Bool
isKingInCheck rules size board color =
  case findKingPos board color of
    Nothing -> False -- No king found (shouldn't happen with validator), so not in check
    Just kingPos ->
      let
        opponent = if color == White then Black else White
        
        -- Get all pieces belonging to the opponent
        enemyPieces = filter (\(pos, p) -> pColor p == opponent) (Map.toList board)
        
        -- Check if ANY enemy piece can move to the King's square
        canHitKing (enemyPos, _) = 
            kingPos `elem` (getValidMoves rules size board enemyPos)
      in
        any canHitKing enemyPieces


-- safe moves means any move that wont cause the king can be captured, or neglecting a check condition
getSafeMoves :: RuleMap -> BoardSize -> Board -> Position -> [Position]
getSafeMoves rules size board fromPos =
  let
    -- 1. Get all physically possible moves 
    rawMoves = getValidMoves rules size board fromPos
    
    -- 2. Define the piece moving
    piece = board Map.! fromPos
    player = pColor piece

    -- 3. Filter: Only keep moves that result in a "Safe" board
    isMoveSafe toPos =
      let
        -- Simulate the move
        tempBoard = Map.insert toPos piece (Map.delete fromPos board)
      in
        -- If King is NOT in check on the new board, the move is safe
        not (isKingInCheck rules size tempBoard player)
  in
    filter isMoveSafe rawMoves


hasLegalMoves :: RuleMap -> BoardSize -> Board -> Color -> Bool
hasLegalMoves rules size board player =
  let
    myPieces = filter (\(_, p) -> pColor p == player) (Map.toList board)
    
    hasMoves (pos, _) = not (null (getSafeMoves rules size board pos))
  in
    any hasMoves myPieces
