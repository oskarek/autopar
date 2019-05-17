# autopar

## Modules

### app/Main.hs
Contains test code, for just trying out the library.

### src/Autopar.hs
Contains the actual library code, with the automatically parallelized list functions.

## Run instructions
Simply run
```
stack build
```
to build the project, and then run
```
stack exec autopar-exe < testinput.txt
```
to run the program (using all available cores and `testinput.txt` as test input).

Use
```
stack exec autopar-exe -- +RTS N4 -s < testinput.txt
```
for example, to run on 4 cores and get timing stats printed after execution.