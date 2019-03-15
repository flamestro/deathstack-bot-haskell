# dsBotHaskell
Bot for DeathStacks \n \n

This is a bot written in Haskell. \n
It was a university project, which I wanted to share. \n
This bot uses fen notation. \n
Fen notation contains gamefield information AND the next player. \n
As example "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r" \n
stand for red player next and GameField : \n
6[rr,rr,rr,rr,rr,rr] \n
5[  ,  ,  ,  ,  ,  ] \n
4[  ,  ,  ,  ,  ,  ] \n
3[  ,  ,  ,  ,  ,  ] \n
2[  ,  ,  ,  ,  ,  ] \n
1[bb,bb,bb,bb,bb,bb] \n
  a  b  c  d  e  f \n
Two characters are reserved for players: 'b' for blue and 'r' for red \n
This bot has two main functionalities: \n
listMoves fenNotation \n
-> prints possible moves into the console \n
-> Moves are printed in this form : a1-2-a3 etc \n
-> console print is similiar to "[startposition-steps-endposition,.....,.....,....]" \n
getMove fenNotation \n
-> prints only one move into the console \n
-> printed in this form : "[a1-2-a3]" \n
-> the move which is printed, is choosen by a implementation of the minimax algorithm and chooses the best move [best for this implementation] \n


