# Bottom Up BGL

## Prerequisites
* ghc - download the installer
* cabal - comes with ghc

## Generic Build
* cabal v2-build

## Generic Execute
* cabal new-exec BottomUp <filename>
* ghci OutputCode.hs

## Windows Executable Execute
* ./BoGL-Compiler.exe <filename>
* ghci OutputCode.hs

## Linux Install
* ./build.sh

## Linux Execute
* ./exec.sh filename

## GHCi
Once the compiled program is in GHCi to run it execute the result function. Other functions can be tested, but these will depend on the implementation in the .bgl file.

## VS Code
There is an automation to build and run the testing file in VS code for development purposes. To run this using VSCode open the folder and then press Ctrl + Shift + B.
