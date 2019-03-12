--- module (NICHT AENDERN!)
module DeathStacksBot where
--- imports (NICHT AENDERN!)
import Data.Char
--- YOUR IMPLEMENTATION STARTS HERE ---

-- Mein Code funktioniert ohne Data.utils und Listmoves und getMove sind unter den TypClasses definiert

-- Dienen zur abstraktion des Codes
width_height  = 6
-- Nur fuer gerade Zahlen sicher getestet verhalten fuer Ungerade Zahlen vielleicht nicht wie erwuenscht 
-- Ab 6 dauert der Algorithmus knapp ueber 4 Minuten ich gehe ein Risiko ein mit der Iterationstiefe 8 und hoffe mein Bot siegt sonst
-- wuerde ich es auf 6 setzen, da die Werte immernoch gut approximiert sind und es so nur knapp eine Minute dauert
-- EDIT: Auf 6 gesetzt, da manche moegliche Boards bei mir zu lange brauchen
iterationsTiefe = 8

data Move = Move {start::String, schritte::Int,end::String}
data Moves = Moves Move Moves | EmptyMove
--MoveEvals speichern einen Move, das Spielbrett NACH dem Move und eine Bewertung nach einer implementierten bewertungsfunktion
data MoveEval = MoveEval {move::Move,spielbrett:: Spielbrett, bewertung::Int}
data Spielbrett = Spielbrett {kachel::Feld, spielfeld:: Spielbrett} | EmptyFeld
data Feld = Kachel {spalte::Int,zeile::Int} | KachelmitValue {spalte::Int,zeile::Int,value::String} 

--dient dazu das Feld aus das ein Move fuehrt zu bestimmen
class ConvtoKachel a where
            kachelOf :: a -> Feld

instance ConvtoKachel Move where 
            kachelOf (Move start schritte end) | head end == 'a' = Kachel 1 (read (drop 1 end))
                                               | head end == 'b' = Kachel 2 (read (drop 1 end))
                                               | head end == 'c' = Kachel 3 (read (drop 1 end))
                                               | head end == 'd' = Kachel 4 (read (drop 1 end))
                                               | head end == 'e' = Kachel 5 (read (drop 1 end))
                                               | head end == 'f' = Kachel 6 (read (drop 1 end))
instance Show Move where
           show (Move start schritte end)  =  start++"-"++(show schritte)++"-"++end

instance Show Moves where
            show (Moves aktuell rest) = (show aktuell)++","++(show rest)  
            show EmptyMove = ""
instance Show MoveEval where 
            show (MoveEval move sf bewertung) = show move ++":"++show sf++":"++ show bewertung++"--------------------------------------------"
instance Eq MoveEval where
            (==) mEval1 mEval2  =  bewertung mEval1 == bewertung mEval2
instance Ord MoveEval where
            (>)  mEval1 mEval2  =  bewertung mEval1 >  bewertung mEval2
            (<)  mEval1 mEval2  =  bewertung mEval1 <  bewertung mEval2
            (<=) mEval1 mEval2  =  bewertung mEval1 <= bewertung mEval2
            (>=) mEval1 mEval2  =  bewertung mEval1 >= bewertung mEval2
instance Show Feld where
           show (Kachel spalte zeile) = nshow spalte ++ show zeile
                             where nshow eingabe  | eingabe == 1 = "a"
                                                  | eingabe == 2 = "b"
                                                  | eingabe == 3 = "c"
                                                  | eingabe == 4 = "d"
                                                  | eingabe == 5 = "e"
                                                  | eingabe == 6 = "f"
                                                  | otherwise    = "x"
           show (KachelmitValue spalte zeile value) = value++"["++(show (Kachel spalte zeile))++"]"

instance Show Spielbrett where
                    show (Spielbrett k sf) = show k ++ "  " ++ show sf
                    show (EmptyFeld)       = ""
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------                    
-- ein einfacher sort algorithmus aus dem Haskell Wiki--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
----------Zielfunktionen ListMoves und getMove  --------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------        
-- diese Funktion kombiniert die anderen Funktionen um so alle möglichen moves auszugeben
listMoves :: String -> String
listMoves eingabe  = movesAsString
               where fen              = (init (init eingabe))
                     brett            = konvFenzuSpielbrett fen
                     brettGefiltert   = filterSpielBrett brett player
                     player           = last eingabe
                     moves            = calculateMoves brett player
                     movesNoRedundant = killRedundantMoves moves
                     movesTooTallRule = filterTooTall movesNoRedundant brettGefiltert
                     movesAsString    = "["++(init (show movesTooTallRule))++"]"



--gibt mit benutzung aller anderen Methoden den besten Move zurück
--getMove "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/,,,,r,bb r"
getMove :: String -> String
getMove eingabe = show (move bestMove) 
                where fen              = (init (init eingabe))
                      brett            = konvFenzuSpielbrett fen
                      brettGefiltert   = filterSpielBrett brett player
                      player           = last eingabe
                      moves            = calculateMoves brett player
                      movesNoRedundant = killRedundantMoves moves
                      movesTooTallRule = filterTooTall movesNoRedundant brettGefiltert
                      allEvals         = allMoveEvals movesTooTallRule player brett
                      bestMove         = absoluteBestMove allEvals
   
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--Eigentlich sollte hier der MinMax Algorithmus implementiert werden aber ich habe es nicht in der zeit geschafft --------------------------
--daher wird nur nicht berechnet ob der Zug auch für folgende Züge am schlausten ist sondern ganz nach dem Greedy---------------------------
--Algorithmus agiert und der aktuell am besten scheinende Zug genommen und wieder zurückgegeben     ----------------------------------------
--EDIT: Der Code funktioniert jetzt doch und stellt den MINIMAX ALGORITHMUS dar, also wird dieser verwendet             --------------------
--------------------------------------------------------------------------------------------------------------------------------------------
                                                  
-- wandelt die uebergebenen Moves passend zu dem Spieler in MoveEvals um 
allMoveEvals :: Moves -> Char->Spielbrett ->[MoveEval]
allMoveEvals EmptyMove player sf = []
allMoveEvals (Moves m rest) player sf = (valMoves m player sf) : ( allMoveEvals rest player sf)

--vereinfacht das verwenden von valMovesX ( Tailrekursion ) und gibt direkt MoveEvals zurueck
valMoves::  Move -> Char -> Spielbrett-> MoveEval
valMoves move player sf = (MoveEval move (afterMove move sf) (valMovesX move sf player player iterationsTiefe))


--Berechnet mit dem MinMax Algorithmus den Absoluten Rang eines Moves ( Absolut : auch mit Hinsicht auf zukuenftige Spielzustaende)
valMovesX:: Move -> Spielbrett-> Char -> Char -> Int-> Int
valMovesX m sf playerstart playeriter 0            = (bewertung (valueMove playerstart m sf))
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
                                                          brettGefiltert   = filterSpielBrett (afterMove m sf) (changePlayer playeriter)
                                                          movesTooTallRule = filterTooTall movesNoRedundant brettGefiltert
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------


-- Testet ob ein Spielbrett schon einen beendeten Zustand aufweist (für den jeweiligen Spieler
boardFinished :: Spielbrett -> Char -> Bool
boardFinished  EmptyFeld _                                                                       = True
boardFinished ( Spielbrett (KachelmitValue spalte zeile value) rest) player | (length value) >=1 = ((head value) == player) && boardFinished rest player
                                                                            | otherwise          = boardFinished rest player

-- Ermöglicht Moves für ein Spielbrett eines Players in MoveEvals zu wandeln
movesAsEvalList:: Moves -> Char-> Spielbrett-> [MoveEval]
movesAsEvalList EmptyMove player sf= []
movesAsEvalList (Moves move rest) player sf | boardFinished (afterMove move sf) player  =  ((MoveEval move (afterMove move sf) 500) : (movesAsEvalList rest player sf))
                                            | otherwise = ((valueMove player move (afterMove move sf)) : (movesAsEvalList rest player sf))

-- gibt den Move mit der besten bewertung zurück wobei die liste MoveEval hierbei am besten schon MoveEvals mit bewertungen nach dem MinMax Algorithmus hat
absoluteBestMove:: [MoveEval] -> MoveEval
absoluteBestMove m = ((bestMoveEval m 0 (MoveEval (Move "x1" 1 "x1") EmptyFeld (0))))


-- gibt den Besten MoveEval in einer Liste von MoveEvals zurück wobei bester sich auf die bewertung bezieht
bestMoveEval:: [MoveEval]-> Int -> MoveEval ->MoveEval
bestMoveEval []     s m                     = m
bestMoveEval (x:xs) s m| (bewertung x >  s) =  bestMoveEval xs (bewertung x) x
bestMoveEval (x:xs) s m| (bewertung x <= s) =  bestMoveEval xs s m

-- Bestimmt aus einer Liste mit MoveEvals die zweit bestbewerteten MoveEvals und gibt diese in einer Liste zurück
bestMoves:: [MoveEval] -> [MoveEval]
bestMoves movesEval = drop (laengeListe-2) (quicksort movesEval)
                        where laengeListe =  length (quicksort movesEval)

-- Nimmt aus einer Liste mit MoveEvals ( welche mit bestMoves bearbeitet wurde ) den besseren Moveeval ( funktioniert nicht so abstrakt wie bestMoveEval)
bestMove:: [MoveEval] -> MoveEval
bestMove   movesEval  =  last (quicksort movesEval)
            
            
-- Bewertet Spielfelder nach folgender Regel :
-- Jeder Stein des übergebenen Spielers zaehlt 10 bis auf die steine auf tooTall Stapeln ( die ersten vier 10 rest 5 ) 
valueBoard:: Spielbrett ->Char -> Int
valueBoard sf player = case sf of 
                            EmptyFeld                                           -> 0
                            Spielbrett (KachelmitValue spalte zeile value) rest -> if   val==player 
                                                                                   then (wert value)+ (valueBoard rest player) 
                                                                                   else (valueBoard rest player)
                                                                                     where wert x | (length x) < 5    = ((length x) *10)
                                                                                                  | otherwise         = 40 + (((length x)-4)*5)
                                                                                           val    | length value >= 1 = head value
                                                                                                  | otherwise         = 'x'
-- bewertet einen Move mit und gibt ein MoveEval zurueck
valueMove :: Char -> Move -> Spielbrett -> MoveEval
valueMove player move sf = (MoveEval move sf (valueBoard (afterMove move sf) player))
    
-------------------------------------------------------------------------------------
----Zur Bestimmung des Spielfelds, nachdem ein Move gemacht wurde     ---------------
-------------------------------------------------------------------------------------

--aendert die startkachel auf den neuen wert nach move
afterMoveStart :: Move -> Spielbrett -> Spielbrett
afterMoveStart move ( Spielbrett (KachelmitValue spalte zeile value) rest) |  start move == (show (Kachel spalte zeile )) = (Spielbrett (KachelmitValue spalte zeile newVal) rest)
                                                                           |  otherwise                                   = (Spielbrett (KachelmitValue spalte zeile value) (afterMoveStart move rest))
                                                                                where newVal = drop (schritte move) value
-- fuegt die bewegten steine auf die neue endkachel
afterMoveEnd:: Move -> Spielbrett -> Spielbrett ->  Spielbrett
afterMoveEnd move EmptyFeld sf                                             = (Spielbrett (KachelmitValue (spalte (kachelOf move)) (zeile (kachelOf move)) (searchVal move sf)) EmptyFeld)
afterMoveEnd move (Spielbrett (KachelmitValue spalte zeile value) rest) sf |  end move == (show (Kachel spalte zeile )) = (Spielbrett (KachelmitValue spalte zeile ((searchVal move sf)++value)) rest)
                                                                           |  otherwise                                 = (Spielbrett (KachelmitValue spalte zeile value) (afterMoveEnd move rest sf))
-- Kompakte Methode um Spielbrett nach einem Move zu erhalten
afterMove :: Move -> Spielbrett -> Spielbrett
afterMove move sf = afterMoveEnd move (afterMoveStart move sf) sf

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

--Sucht die zu bewegnden steine auf dem Spielbrett
searchVal :: Move-> Spielbrett -> String
searchVal move ( Spielbrett (KachelmitValue spalte zeile value) rest)  |  start move == (show (Kachel spalte zeile )) = take (schritte move) value
                                                                       |  otherwise                                   = searchVal move rest





--Hier werden die Moves für einen bestimmten spieler für ein vorgebenes Spielfeld bestimmt indem man die
--8 moeglichen Moves (alle kombinationen aus Moeglichen Zeilen und Spalten) für jede Kachel auf der sich Stapel des Spielers befinden in ein Moveset tut
calculateMoves :: Spielbrett -> Char -> Moves 
calculateMoves spielbrett player = case spielbrett of
                                 EmptyFeld                                         -> EmptyMove
                                 Spielbrett (KachelmitValue spalte zeile value) sf -> if (not (null value)) && player == (head value) then combinePossible (KachelmitValue spalte zeile value) (length value) else calculateMoves sf player
                                            where combinePossible (KachelmitValue spalte zeile value) steps | steps-1 > (-1) =  Moves (Move (show (Kachel spalte zeile)) steps (show (Kachel spalte (getPossiblePos zeile steps )))) 
                                                                                                                               (Moves (Move (show (Kachel spalte zeile)) steps (show (Kachel spalte (getPossibleNeg zeile steps )))) 
                                                                                                                               (Moves (Move (show (Kachel spalte zeile)) steps (show (Kachel (getPossiblePos spalte steps ) zeile)))
                                                                                                                               (Moves (Move (show (Kachel spalte zeile)) steps (show (Kachel (getPossibleNeg spalte steps ) zeile)))
                                                                                                                               (Moves (Move (show (Kachel spalte zeile)) steps (show (Kachel (getPossiblePos spalte steps ) (getPossiblePos zeile steps ))))
                                                                                                                               (Moves (Move (show (Kachel spalte zeile)) steps (show (Kachel (getPossibleNeg spalte steps ) (getPossibleNeg zeile steps ))))
                                                                                                                               (Moves (Move (show (Kachel spalte zeile)) steps (show (Kachel (getPossibleNeg spalte steps ) (getPossiblePos zeile steps ))))
                                                                                                                               (Moves (Move (show (Kachel spalte zeile)) steps (show (Kachel (getPossiblePos spalte steps ) (getPossibleNeg zeile steps ))))
                                                                                                                               (combinePossible (KachelmitValue spalte zeile value) (steps-1)))))))))
                                                                                                            | otherwise      =  calculateMoves sf player

--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------HILFSFUNKTIONEN ZUR VERARBEITUNG----------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------


-- Zum Testen ( HUnit wollte nicht funktionieren) 
-- konvFenzuSpielbrett "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" 
-- listMovesasMoves "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r" 
-- getMove "r,b,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb b" 
-- getMove ",,r,,brr,/,,,rb,,/,brbr,,,,/rbbr,b,r,r,bb,/,r,,bb,,r/b,,,,, b" 
-- movesAsEvalList (listMovesasMoves  "rr,b,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb b") 'b' (konvFenzuSpielbrett "rr,b,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb")
-- show (bestMove (movesAsEvalList (listMovesasMoves  "rr,b,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb b") 'b' (konvFenzuSpielbrett "rr,b,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb")))
-- show (bestMoves (movesAsEvalList (listMovesasMoves  "rr,b,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r") 'r' (konvFenzuSpielbrett "rr,b,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb")))
-- listMoves "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r" 


-- Diese Funktion bekommt eine Notation wie in der Aufgabe definiert und gibt alle Moves als Moves zurück 
listMovesasMoves :: String -> Moves 
listMovesasMoves eingabe  = movesTooTallRule
               where fen              = (init (init eingabe))
                     brett            = konvFenzuSpielbrett fen
                     brettGefiltert   = filterSpielBrett brett player
                     player           = last eingabe
                     moves            = calculateMoves brett player
                     movesNoRedundant = killRedundantMoves moves
                     movesTooTallRule = filterTooTall movesNoRedundant brettGefiltert
                     
-- diese Funktion wandelt einen String in fen Notation in ein Spielbrett um

konvFenzuSpielbrett :: String -> Spielbrett
konvFenzuSpielbrett fen = konvFenzuSpielbrettTailRek fen width_height 1

konvFenzuSpielbrettTailRek :: String -> Int -> Int -> Spielbrett
konvFenzuSpielbrettTailRek fen row col | (fen      =="" ) =  EmptyFeld
                                       | (head fen ==',') = (konvFenzuSpielbrettTailRek (tail fen) row (col+1))
                                       | (head fen =='/') = (konvFenzuSpielbrettTailRek (tail fen) (row-1) 1)
                                       |  otherwise       =  Spielbrett (KachelmitValue col row (getKachelVal fen)) (konvFenzuSpielbrettTailRek (nextKachel fen) row col)
                                                                where getKachelVal s  | s      == []  = ""
                                                                                      | head s == 'b' = head s : (getKachelVal (tail s))
                                                                                      | head s == 'r' = head s : (getKachelVal (tail s))
                                                                                      | otherwise     = ""
                                                                      nextKachel s    | s      == []  = ""
                                                                                      | head s == 'b' = nextKachel (tail s)
                                                                                      | head s == 'r' = nextKachel (tail s)
                                                                                      | otherwise     = s
                                    
-- Filtert das Spielbrett auf die Kacheln die mehr als 4 Steine stehen haben
filterSpielBrett :: Spielbrett -> Char -> Spielbrett
filterSpielBrett EmptyFeld _= EmptyFeld
filterSpielBrett (Spielbrett (KachelmitValue spalte zeile value) sf) player | ( length value) > 4 && (head value) ==player = (Spielbrett (KachelmitValue spalte zeile value) EmptyFeld)
                                                                            |  otherwise                                   =  filterSpielBrett sf player
-- bekommt die berechneten gesamt moves und ein Spielbrett, und filtert die Moves passend zu der ersten Kachel des Spielfelds
-- das Spielfeld sollte vorher gefiltert werden denn es wird die tooTall Regel angewandt
filterTooTall :: Moves -> Spielbrett-> Moves 
filterTooTall moves EmptyFeld   =  moves
filterTooTall moves sf          = (filterTooTallX moves (kachel sf))
-- Es werden die Moves überprüft ob sie gültig sind nach der TooTall regel
-- falls ja werden die gültigen Moves bestimmt
filterTooTallX :: Moves -> Feld   -> Moves 
filterTooTallX  EmptyMove _                                             = EmptyMove
filterTooTallX (Moves aktuell rest) (KachelmitValue spalte zeile value)  | (length value)-(schritte aktuell) >4              =  filterTooTallX rest (KachelmitValue spalte zeile value)
                                                                         | (show (Kachel spalte zeile) )== (start aktuell)   = (Moves aktuell (filterTooTallX rest (KachelmitValue spalte zeile value)))
                                                                         | otherwise                                         =  filterTooTallX rest (KachelmitValue spalte zeile value)

--Es werden die Moves übergeben und Redundante Moves entfernt
killRedundantMoves :: Moves -> Moves 
killRedundantMoves moves = killRedundantMovesX moves [] 
-- durch Tail rekursion wird jeder Move der in die Liste seen kommt nur einmal in die Ausgabe genommen 
-- es wird also für jeden Move überprüft ob dieser element von seen ist und falls ja wird der aktuelle Move nicht in das neue Moveset aufgenommen
killRedundantMovesX :: Moves -> [String]  -> Moves 
killRedundantMovesX  EmptyMove _                                                  = EmptyMove
killRedundantMovesX (Moves aktuell rest)  seen | (start aktuell) == (end aktuell) = killRedundantMovesX rest seen
                                               | (show aktuell) `elem` seen       = killRedundantMovesX rest seen
                                               | otherwise                        = (Moves aktuell (killRedundantMovesX rest ((show aktuell):seen)))

--Es werden die moeglichkeiten berechnet auf einer bestimmten Zeile/Spalte(je nachdem ob pos die zeile oder spalte wiederspiegelt) zu landen,
-- wenn man sich nach oben oder rechts bewegt
getPossiblePosX:: Int -> Int-> Int -> Int  -> Int
getPossiblePosX pos count schritte distance | (distance-1) >(width_height-1) = getPossiblePosX pos (count+1) schritte (distance-(width_height-1)) 
                                            |  otherwise                     = (spiegel (distance-1) count) +1
getPossiblePos:: Int -> Int   -> Int
getPossiblePos pos schritte =getPossiblePosX pos 0 schritte (pos+ schritte) 
--Es werden die moeglichkeiten berechnet auf einer bestimmten Zeile/Spalte(je nachdem ob pos die zeile oder spalte wiederspiegelt) zu landen,
-- wenn man sich nach unten oder links bewegt
getPossibleNegX:: Int -> Int-> Int -> Int -> Int
getPossibleNegX pos count schritte distance | (distance-1) <0                = (getPossibleNegX pos (count+1) schritte (distance+(width_height-1))) 
                                            |  otherwise                     = (spiegel (distance-1) count) +1
getPossibleNeg:: Int -> Int  -> Int
getPossibleNeg pos schritte  =getPossibleNegX pos 0 schritte (pos- schritte) 
 
--Diese Funktion stellt die Spiegelung an den Raendern des Spielfelds dar hierzu bekommt sie die ermittelte distance, also die Anzahl an Feldern die 
--Die Figuren wirklich bewegt werden, und einen count welcher bestimmt auf welcher Spiegelung sich die Steine befinden
spiegel:: Int -> Int -> Int
spiegel distance count | (count  `mod` 2 ) ==0 = distance 
                       | otherwise             =(width_height-distance -1)

