# Conway's game of life in Fortran90

A simple rendition of Conway's game of life.  This program includes a simple animation subroutine. 

### Compiling 

`gfortran game_of_life.f90 -o game_of_life`

### Running

`./game_of_life` this will clear the console and run an animation.  To exit the animation type `C-c` or `CTRL-c`.

### Separate models

Change the `model` variable number to get seperate models and the `speed` variable to get different speeds (higher speed is slower animation). [The `speed` variable is used to redraw the animation every so many milliseconds.]
