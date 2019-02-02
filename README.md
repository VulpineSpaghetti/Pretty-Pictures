# Pretty Pictures

## Description

A small program that generates interesting images using a simple reduce/fold.

It allows you to make pictures such as: 
![circly water colors](https://i.imgur.com/OwLaGsU.jpg) 
(generated using `makeArtAndSave "Circle" (1280, 720) BottomLeft [(0,0)] 2 pickClosest brightestColor (useShape circle)`)

or

![Pyramid](https://i.imgur.com/KUTTaUz.jpg)
(generated using `makeArtAndSave "Pyramid" (1920, 1080) Center [(0,0)] 2 pickClosest brightestColor . useShape . pickShapeIfYIsNegative (ellipse 5 1) $ afterN 300 (diamond 1 1) (moveShape 400 500 circle)`

## Installation instructions

First you need GHC and Stack. If you don't have either, then you can download them from [here](https://www.haskell.org/platform/).

 * download the project
 * open terminal in the folder of the project (**not** in `src/`)
 * run `stack install` to install
 * then run `Pretty-pictures.exe` on Windows or `Pretty-pictures` on Linux.

After that the program will be run and any pictures will be outputed into an `art/` subfolder of the current folder.

You can configure what kind of picture gets drawn inside `main` by altering the arguments of `makeArtAndSave`