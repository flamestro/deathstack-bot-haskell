# dsBotHaskell
Bot for DeathStacks

This is a bot written in Haskell.
It was a university project, which I wanted to share.
This bot uses fen notation.
Fen notation contains gamefield information AND the next player.
As example "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r" 
stand for red player next and GameField :
6[rr,rr,rr,rr,rr,rr]
5[  ,  ,  ,  ,  ,  ]
4[  ,  ,  ,  ,  ,  ]
3[  ,  ,  ,  ,  ,  ]
2[  ,  ,  ,  ,  ,  ]
1[bb,bb,bb,bb,bb,bb]
  a  b  c  d  e  f
Two characters are reserved for players: 'b' for blue and 'r' for red 
This bot has two main functionalities:
listMoves fenNotation
-> prints possible moves into the console
-> Moves are printed in this form : a1-2-a3 etc
-> console print is similiar to "[startposition-steps-endposition,.....,.....,....]"
getMove fenNotation
-> prints only one move into the console
-> printed in this form : "[a1-2-a3]"
-> the move which is printed, is choosen by a implementation of the minimax algorithm and chooses the best move [best for this implementation]


