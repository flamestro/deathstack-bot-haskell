# dsBotHaskell
Bot for DeathStacks <br>

This is a bot written in Haskell.  <br>
It was a university project, which I wanted to share.  <br>
This bot uses fen notation.  <br>
Fen notation contains gamefield information AND the next player.  <br>
As example "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r"  <br> 
stand for red player next and GameField :  <br>
6[rr,rr,rr,rr,rr,rr]  <br>
5[  ,  ,  ,  ,  ,  ]  <br>
4[  ,  ,  ,  ,  ,  ]  <br>
3[  ,  ,  ,  ,  ,  ]  <br>
2[  ,  ,  ,  ,  ,  ]  <br>
1[bb,bb,bb,bb,bb,bb]  <br>
  a  b  c  d  e  f  <br>
Two characters are reserved for players: 'b' for blue and 'r' for red  <br>
This bot has two main functionalities:  <br>
listMoves fenNotation  <br>
-> prints possible moves into the console  <br>
-> Moves are printed in this form : a1-2-a3 etc  <br>
-> console print is similiar to "[startposition-steps-endposition,.....,.....,....]"  <br>
getMove fenNotation  <br>
-> prints only one move into the console  <br>
-> printed in this form : "[a1-2-a3]"  <br>
-> the move which is printed, is choosen by a implementation of the minimax algorithm and chooses the best move [best for this implementation]  <br>


