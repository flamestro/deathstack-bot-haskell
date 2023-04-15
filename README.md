# DeathStack AI
This is a bot for a not so well game called DeathStack.  
The AI uses the minmax algorithm to determine which moves are the best.

To make it feasible to run, it will only check up to X layers in the tree of possible moves.

It will use [Forsth-Edwards Notation (FEN)](https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation)

The notation string contains gamefield information and the next player.  
As an example FEN string: `"rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb r"`

As the notation ends with a whitespace followed by an r, this means that the next player is red.

The graphical mapping might look like this:

<table>
  <tr>
<th></th>
<th>a</th>
<th>b</th>
<th>c</th>
<th>d</th>
<th>e</th>
<th>f</th>

  </tr>
<tr> 
<td> 	6</td> 
<td> rr</td>
<td> rr</td>
<td> rr</td>
<td> rr</td>
<td> rr</td>
<td> rr</td>

</tr>
<tr> 
<td> 	5</td> 
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>

</tr>
<tr> 
	<td> 	4</td> 
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>

</tr>
<tr> 
	<td> 	3</td> 
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>

</tr>
<tr> 
	<td> 	2</td> 
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>
<td> </td>

</tr>
<tr> 
	<td> 	1</td> 
<td> bb</td>
<td> bb</td>
<td> bb</td>
<td> bb</td>
<td> bb</td>
<td> bb</td>

</tr>

</table>

Two characters are reserved for players: 'b' for blue and 'r' for red  <br>
This bot has two main functionalities:  
listMoves fenNotation  
- prints possible moves into the console    
- Moves are printed in this form : a1-2-a3 etc    
- console print is similiar to "[startposition-steps-endposition,.....,.....,....]"   
getMove fenNotation  
- prints only one move into the console   
- printed in this form : "[a1-2-a3]"   
- the move which is printed, is choosen by a implementation of the minimax algorithm and chooses the best move [best for this implementation]   


