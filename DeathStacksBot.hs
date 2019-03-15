module DeathStacksBot where
import Data.Char


-- The Width and the heigth of a Gamefield
width_height  = 6
-- depth of minimax algorithm (shouldnt be higher than 8 (takes ~5 mins) 
depth = 4

data Move      = Move {start::String, steps::Int,end::String}
data Moves     = Moves Move Moves | EmptyMove
--MoveEvals save informations about a Move, a gameField after the Move and a rating
data MoveEval  = MoveEval {move::Move,gameField:: GameField, rating::Int}
data GameField = GameField {field::Field, spielfeld:: GameField} | EmptyField
data Field     = Field {column::Int,row::Int} | FieldwValue {column::Int,row::Int,value::String} 

--this class is created for Move ( to convert a moves endPos into a field 
class ConvtoKachel a where
            kachelOf :: a -> Field

instance ConvtoKachel Move where 
            kachelOf (Move start steps end)    | head end == 'a' = Field 1 (read (drop 1 end))
                                               | head end == 'b' = Field 2 (read (drop 1 end))
                                               | head end == 'c' = Field 3 (read (drop 1 end))
                                               | head end == 'd' = Field 4 (read (drop 1 end))
                                               | head end == 'e' = Field 5 (read (drop 1 end))
                                               | head end == 'f' = Field 6 (read (drop 1 end))
instance Show Move where
           show (Move start steps end)  =  start++"-"++(show steps)++"-"++end

instance Show Moves where
            show (Moves actual rest) = (show actual)++","++(show rest)  
            show EmptyMove = ""
instance Show MoveEval where 
            show (MoveEval move sf rating) = show move ++":"++show sf++":"++ show rating++"--------------------------------------------"
instance Eq MoveEval where
            (==) mEval1 mEval2  =  rating mEval1 == rating mEval2
instance Ord MoveEval where
            (>)  mEval1 mEval2  =  rating mEval1 >  rating mEval2
            (<)  mEval1 mEval2  =  rating mEval1 <  rating mEval2
            (<=) mEval1 mEval2  =  rating mEval1 <= rating mEval2
            (>=) mEval1 mEval2  =  rating mEval1 >= rating mEval2
instance Show Field where
           show (Field column row) = nshow column ++ show row
                             where nshow input    | input == 1 = "a"
                                                  | input == 2 = "b"
                                                  | input == 3 = "c"
                                                  | input == 4 = "d"
                                                  | input == 5 = "e"
                                                  | input == 6 = "f"
                                                  | otherwise    = "x"
           show (FieldwValue column row value) = value++"["++(show (Field column row))++"]"

instance Show GameField where
                    show (GameField k sf) = show k ++ "  " ++ show sf
                    show (EmptyField)       = ""

--------------------------------------------------------------------------------------------------------------------------------------------                    
-- a simple sort algortihm 							  --------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------- 

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

--------------------------------------------------------------------------------------------------------------------------------------------
----------listMoves und getMove  				--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
     
-- returns every possible Move after inputing a completeFen
-- complete fen :: String [a.e. "rr,rr,rr,rr,rr/,r,,rr,,rr/rrr,rrr,r,,rrr,r r"]
listMoves :: String -> String
listMoves input  = movesAsString
               where fen              = (init (init input))
                     field            = convFenToGamefield fen
                     fieldfiltered   = filterGameField field player
                     player           = last input
                     moves            = calculateMoves field player
                     movesNoRedundant = killRedundantMoves moves
                     movesTooTallRule = filterTooTall movesNoRedundant fieldfiltered
                     movesAsString    = "["++(init (show movesTooTallRule))++"]"



--returns the best rated Move of every possible Move
--getMove "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/,,,,r,bb r"
getMove :: String -> String
getMove input = show (move bestMoveCal) 
                where fen              = (init (init input))
                      field            = convFenToGamefield fen
                      fieldfiltered    = filterGameField field player
                      player           = last input
                      moves            = calculateMoves field player
                      movesNoRedundant = killRedundantMoves moves
                      movesTooTallRule = filterTooTall movesNoRedundant fieldfiltered
                      allEvals         = allMoveEvals movesTooTallRule player field
                      bestMoveCal      = bestMove allEvals
   

--------------------------------------------------------------------------------------------------------------------------------------------
--own implementation of the minimax algorithm ----------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------

-- converts Moves into a MoveEval List but with absolute ratings using valMoves
allMoveEvals :: Moves -> Char->GameField ->[MoveEval]
allMoveEvals EmptyMove player sf = []
allMoveEvals (Moves m rest) player sf = (valMoves m player sf) : ( allMoveEvals rest player sf)

--returns a MoveEval with a absolute rating for a given Move
--calculates a absolute value for a move, with forshadowing future moves (minimax algorithm)
valMoves::  Move -> Char -> GameField-> MoveEval
valMoves move player sf = (MoveEval move (afterMove move sf) (valMovesX move sf player player depth))

valMovesX:: Move -> GameField-> Char -> Char -> Int-> Int
valMovesX m sf playerstart playeriter 0            = (rating (valueMove playerstart m sf))
                                                            where changePlayer 'r' = 'b'
                                                                  changePlayer 'b' = 'r'
valMovesX m sf playerstart playeriter iteration    | boardFinished (afterMove m sf) playeriter = 10000 +(10^iteration)
                                                   | max && leftNode >  rightNode = leftNode+(10^iteration)
                                                   | max && leftNode <= rightNode = rightNode+(10^iteration)
                                                   | min && leftNode >  rightNode = rightNode-(10^iteration)
                                                   | min && leftNode <= rightNode = leftNode-(10^iteration)                                                 
                                                    where min =  playeriter /= playerstart
                                                          max =  not min
                                                          bestMovesList = bestMoves (movesAsEvalList movesTooTallRule (changePlayer playeriter) (afterMove m sf))
                                                          changePlayer 'r' = 'b'
                                                          changePlayer 'b' = 'r'
                                                          leftNode         | length bestMovesList >=1 = (valMovesX (move (head bestMovesList)) (afterMove m sf) playerstart (changePlayer playeriter) (iteration-1))
                                                                           | max                = 1000    
                                                                           | min                = 0
                                                          rightNode        | length bestMovesList >=1 = (valMovesX (move (last bestMovesList)) (afterMove m sf) playerstart (changePlayer playeriter) (iteration-1))
                                                                           | max                = 1000    
                                                                           | min                = 0
                                                          moves            = calculateMoves (afterMove m sf) (changePlayer playeriter)
                                                          movesNoRedundant = killRedundantMoves moves
                                                          brettGefiltert   = filterGameField (afterMove m sf) (changePlayer playeriter)
                                                          movesTooTallRule = filterTooTall movesNoRedundant brettGefiltert
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------


-- tests if a GameField is already finished
boardFinished :: GameField -> Char -> Bool
boardFinished  EmptyField _                                                                      = True
boardFinished ( GameField (FieldwValue column row value) rest) player      | (length value) >=1 = ((head value) == player) && boardFinished rest player
                                                                           | otherwise          = boardFinished rest player

-- Converts Moves for a GameField into a List of MoveEvals
movesAsEvalList:: Moves -> Char-> GameField-> [MoveEval]
movesAsEvalList EmptyMove player sf= []
movesAsEvalList (Moves move rest) player sf | boardFinished (afterMove move sf) player  =  ((MoveEval move (afterMove move sf) 500) : (movesAsEvalList rest player sf))
                                            | otherwise = ((valueMove player move (afterMove move sf)) : (movesAsEvalList rest player sf))


-- gets a MoveEval List and returns the two best rated of them
bestMoves:: [MoveEval] -> [MoveEval]
bestMoves movesEval = drop (laengeListe-2) (quicksort movesEval)
                        where laengeListe =  length (quicksort movesEval)

-- gets a list of MoveEvals and returns the one with the best rating
bestMove:: [MoveEval] -> MoveEval
bestMove   movesEval  =  last (quicksort movesEval)
            
            
-- rates gamefield by following ruleset :
--every stone of the given player adds 10 to the complete point count, but if there are more than 4 stones on a position,
--the points for this position are counted 10 for the first 4 stones and 5 for every stone afterwards
valueBoard:: GameField ->Char -> Int
valueBoard sf player = case sf of 
                            EmptyField                                           -> 0
                            GameField (FieldwValue column row value) rest -> if   val==player 
                                                                                   then (wert value)+ (valueBoard rest player) 
                                                                                   else (valueBoard rest player)
                                                                                     where wert x | (length x) < 5    = ((length x) *10)
                                                                                                  | otherwise         = 40 + (((length x)-4)*5)
                                                                                           val    | length value >= 1 = head value
                                                                                                  | otherwise         = 'x'
-- values a Move and returns a MoveEval Data
valueMove :: Char -> Move -> GameField -> MoveEval
valueMove player move sf = (MoveEval move sf (valueBoard (afterMove move sf) player))
    
-------------------------------------------------------------------------------------
----Calculation of a GameField after a given Move is made 		      ---------------
-------------------------------------------------------------------------------------


--this method returns a GameField Data after a move is made 
afterMove :: Move -> GameField  -> GameField 
afterMove move sf = afterMoveEnd move (afterMoveStart move sf) sf

--changes the endPos on the gameField
afterMoveEnd:: Move -> GameField -> GameField  ->  GameField 
afterMoveEnd move EmptyField sf                                             = (GameField  (FieldwValue (column (kachelOf move)) (row (kachelOf move)) (searchVal move sf)) EmptyField)
afterMoveEnd move (GameField  (FieldwValue column row value) rest) sf      |  end move == (show (Field column row ))    = (GameField  (FieldwValue column row ((searchVal move sf)++value)) rest)
                                                                           |  otherwise                                 = (GameField  (FieldwValue column row value) (afterMoveEnd move rest sf))
--changes the startPos on the gameField
afterMoveStart :: Move -> GameField -> GameField 
afterMoveStart move ( GameField (FieldwValue column row value) rest)      |  start move == (show (Field column row ))    = (GameField (FieldwValue column row newVal) rest)
                                                                          |  otherwise                                   = (GameField (FieldwValue column row value) (afterMoveStart move rest))
                                                                                where newVal = drop (steps move) value
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

--searches the stones, which have to be moved
--parameters Move and GameField
searchVal :: Move-> GameField  -> String
searchVal move ( GameField  (FieldwValue column row value) rest)       |  start move == (show (Field column row ))    = take (steps move) value
                                                                       |  otherwise                                   = searchVal move rest

--calculates every possible move for a given GameField and a given Player and returns Moves Data
calculateMoves :: GameField  -> Char -> Moves 
calculateMoves gameField  player = case gameField of
                                 EmptyField                                         -> EmptyMove
                                 GameField  (FieldwValue column row value) sf -> if (not (null value)) && player == (head value) then combinePossible (FieldwValue column row value) (length value) else calculateMoves sf player
                                            where combinePossible (FieldwValue column row value) steps  | steps-1 > (-1) =  Moves (Move (show (Field column row)) steps (show (Field column (getPossiblePos row steps )))) 
                                                                                                                               (Moves (Move (show (Field column row)) steps (show (Field column (getPossibleNeg row steps )))) 
                                                                                                                               (Moves (Move (show (Field column row)) steps (show (Field (getPossiblePos column steps ) row)))
                                                                                                                               (Moves (Move (show (Field column row)) steps (show (Field (getPossibleNeg column steps ) row)))
                                                                                                                               (Moves (Move (show (Field column row)) steps (show (Field (getPossiblePos column steps ) (getPossiblePos row steps ))))
                                                                                                                               (Moves (Move (show (Field column row)) steps (show (Field (getPossibleNeg column steps ) (getPossibleNeg row steps ))))
                                                                                                                               (Moves (Move (show (Field column row)) steps (show (Field (getPossibleNeg column steps ) (getPossiblePos row steps ))))
                                                                                                                               (Moves (Move (show (Field column row)) steps (show (Field (getPossiblePos column steps ) (getPossibleNeg row steps ))))
                                                                                                                               (combinePossible (FieldwValue column row value) (steps-1)))))))))
                                                                                                        | otherwise      =  calculateMoves sf player

--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------Helping Functions-------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------

-- converts a fenString into a GameField
convFenToGamefield :: String -> GameField
convFenToGamefield fen = convFenToGamefieldRek fen width_height 1

convFenToGamefieldRek :: String -> Int -> Int -> GameField
convFenToGamefieldRek fen row col | (fen      =="" ) =  EmptyField
                                  | (head fen ==',') = (convFenToGamefieldRek (tail fen) row (col+1))
                                  | (head fen =='/') = (convFenToGamefieldRek (tail fen) (row-1) 1)
                                  |  otherwise       =  GameField (FieldwValue col row (getKachelVal fen)) (convFenToGamefieldRek (nextKachel fen) row col)
                                                                where getKachelVal s  | s      == []  = ""
                                                                                      | head s == 'b' = head s : (getKachelVal (tail s))
                                                                                      | head s == 'r' = head s : (getKachelVal (tail s))
                                                                                      | otherwise     = ""
                                                                      nextKachel s    | s      == []  = ""
                                                                                      | head s == 'b' = nextKachel (tail s)
                                                                                      | head s == 'r' = nextKachel (tail s)
                                                                                      | otherwise     = s
                                    
-- filters a GameField to fields with more then 4 stones 
filterGameField :: GameField -> Char -> GameField
filterGameField EmptyField _= EmptyField
filterGameField (GameField (FieldwValue column row value) sf) player        | ( length value) > 4 && (head value) ==player = (GameField (FieldwValue column row value) EmptyField)
                                                                            |  otherwise                                   =  filterGameField sf player
-- search a gamefield for a TooTall Field, if one exists, then it will be returned [ should be used with filtered fields only]
filterTooTall :: Moves -> GameField-> Moves 
filterTooTall moves EmptyField   =  moves
filterTooTall moves sf          = (filterTooTallX moves (field sf))

filterTooTallX :: Moves -> Field   -> Moves 
filterTooTallX  EmptyMove _                                            = EmptyMove
filterTooTallX (Moves actual rest) (FieldwValue column row value)      | (length value)-(steps actual) >4              =  filterTooTallX rest (FieldwValue column row value)
                                                                       | (show (Field column row) )== (start actual)      = (Moves actual (filterTooTallX rest (FieldwValue column row value)))
                                                                       | otherwise                                         =  filterTooTallX rest (FieldwValue column row value)

--This function gets Moves as parameter and and deletes duplicates 
killRedundantMoves :: Moves -> Moves 
killRedundantMoves moves = killRedundantMovesX moves [] 

killRedundantMovesX :: Moves -> [String]  -> Moves 
killRedundantMovesX  EmptyMove _                                                  = EmptyMove
killRedundantMovesX (Moves actual rest)  seen  | (start actual) == (end actual)   = killRedundantMovesX rest seen
                                               | (show actual) `elem` seen        = killRedundantMovesX rest seen
                                               | otherwise                        = (Moves actual (killRedundantMovesX rest ((show actual):seen)))

--this method calculates possible new positions for moves to the rigth and up
getPossiblePosX:: Int -> Int-> Int -> Int  -> Int
getPossiblePosX pos count steps distance    | (distance-1) >(width_height-1) = getPossiblePosX pos (count+1) steps (distance-(width_height-1)) 
                                            |  otherwise                     = (reflection (distance-1) count) +1
getPossiblePos:: Int -> Int   -> Int
getPossiblePos pos steps =getPossiblePosX pos 0 steps (pos+ steps) 
--this method calculates possible new positions for moves to the left and down
getPossibleNegX:: Int -> Int-> Int -> Int -> Int
getPossibleNegX pos count steps distance    | (distance-1) <0                = (getPossibleNegX pos (count+1) steps (distance+(width_height-1))) 
                                            |  otherwise                     = (reflection (distance-1) count) +1
getPossibleNeg:: Int -> Int  -> Int
getPossibleNeg pos steps  =getPossibleNegX pos 0 steps (pos- steps) 
 
--this method helps calculating positions after reflection
reflection:: Int -> Int -> Int
reflection distance count | (count  `mod` 2 ) ==0 = distance 
                          | otherwise             =(width_height-distance -1)