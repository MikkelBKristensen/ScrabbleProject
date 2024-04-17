# ScrabbleProject
This is a Scrabble Project

##How to manually input a move
Input move (format '(<x-coordinate><y-coordinate> <piece id><character>
<point-value> )*', note the absence of space between the last inputs)

Example:
0 0 4D2 0 1 0O0

##Log 17th of April
Today we "finalized" making our dictionary(Trie), and added it to the wrapper in progam.cs.
We also found that in Scrabble.fs the "let move = RegEx.parseMove input" should be replaced by a function 
that returns a move, in order to play the game "automatically". 
For now we have not added anything to the state. 

