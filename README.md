# Bottom Up BGL

## Prerequisites
* ghc - download the installer
* cabal - comes with ghc (Might have to be installed manually, package usually called cabal-install)

## Linux Install
* ./build.sh

## Linux Execute
* ./exec.sh filename

## Generic Build (For newer cabal versions)
* cabal v2-build

## Generic Execute
* cabal new-exec BottomUp <filename>
* Consider running the files testing.bgl, notakto.bgl and countGame1.bgl as they are working test files.
* The command would look like cabal new-exec BottomUp test/testing.bgl
* ghci OutputCode.hs

## Windows Executable Execute
* ./BoGL-Compiler.exe <filename>
* ghci OutputCode.hs

## GHCi
Once the compiled program is in GHCi to run it execute the result function. Other functions can be tested, but these will depend on the implementation in the .bgl file.

## VS Code
There is an automation to build and run the testing file in VS code for development purposes. To run this using VSCode open the folder and then press Ctrl + Shift + B.

## Formal Language Definition
See https://github.com/The-Code-In-Sheep-s-Clothing/Example-DSTL for a formal language definition
