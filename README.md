# Bottom Up BGL

## Prerequisites
* ghc (Most version should work)- download the installer
* cabal (Versions 3)- comes with ghc (Might have to be installed manually, package usually called cabal-install)
  * comand to update is: `cabal install Cabal cabal-install`
  * if cabal is installed with apt-get you will need to add .cabal/bin to the start of the path

## Linux Install
* ./build.sh

## Linux Execute
* ./exec.sh filename
* Consider running the files testing.bgl, notakto.bgl and countGame1.bgl as they are working test files.

## Generic Build (For newer cabal versions)
* cabal update
* cabal new-build

## Generic Execute
* cabal new-exec BottomUp <filename>
* Consider running the files testing.bgl, notakto.bgl and countGame1.bgl as they are working test files.
* The command would look like cabal new-exec BottomUp test/testing.bgl
* ghci OutputCode.hs

## Windows Executable Execute
* `./BoGL-Compiler.exe <filename>`
* `ghci OutputCode.hs`

## GHCi
Once the compiled program is in GHCi to run it execute the result function. Other functions can be tested, but these will depend on the implementation in the .bgl file.

## VS Code
There is an automation to build and run the testing file in VS code for development purposes. To run this using VSCode open the folder and then press Ctrl + Shift + B.

## Builtin Functions
The language has a few important builtin functions.
* input - get input from user based on Input type
* inARow : (Int, Player, Board) -> Bool - See if certain number of players on the board
* countBoard : (Content, Board) -> Int - Count the number of a certain piece on the board

## Formal Language Definition
See https://github.com/The-Code-In-Sheep-s-Clothing/Example-DSTL for a formal language definition
