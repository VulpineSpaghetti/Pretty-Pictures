module Main (main) where

import StartingPosition
import IndexPath
import ColorCombinator
import ColorPicker
import Color
import ColorTree
import Pixels

import Data.Ix
import Data.Bifunctor
import Data.List

import Linear.V3
import System.Random
import Graphics.Image hiding (Array, maximum)


main :: IO ()
main = do
    let bounds = (600, 400)
        -- bounds = (1280, 720)
        -- bounds = (1920, 1080)
    
    makeArtAndSave "Circle" bounds TheMiddle [(0,0)] 2 circularPath pickClosestCT maxColor


makeArtAndSave
    :: String
    -> (Int, Int)
    -> StartingPos
    -> [(Int, Int)]
    -> Int
    -> IndexPath [] (Int, Int)
    -> ColorPicker' ColorTree Color
    -> ColorCombinator [] Color
    -> IO ()

makeArtAndSave fileName bounds startingPos relativeSeedLocations searchDistance indexPath colorPicker colorCombinator
    = do
    randomGenerator <- getStdGen

    let art = makeArt bounds randomGenerator startingPos relativeSeedLocations searchDistance indexPath colorPicker colorCombinator
        
        maxRed = maximum . fmap getRed $ art
        maxGreen = maximum . fmap getGreen $ art
        maxBlue = maximum . fmap getBlue $ art
        max = V3 maxRed maxGreen maxBlue
        
        -- Necessary because makeImage seems to take the y coordinate first and x coordinate second
        invertCoords (x,y) = (y,x)
        
        image = makeImage (invertCoords bounds) (toDoublePixel max . maybe 0 id . (art !) . invertCoords) :: Image VS RGB Double

    writeImageExact PNG [] ("art/" ++ fileName ++ ".png") image

    putStrLn ("Finished " ++ fileName ++ "!")



makeArt
    :: RandomGen gen
    => (Int, Int)
    -> gen
    -> StartingPos
    -> [(Int, Int)]
    -> Int
    -> IndexPath [] (Int, Int)
    -> ColorPicker' ColorTree Color
    -> ColorCombinator [] Color
    -> Pixels

makeArt (x,y) randomGenerator startingPos relativeSeedLocations searchDistance indexPath colorPicker colorCombinator
    = makePicture bounds searchDistance orderedIndices colors pixels colorPicker colorCombinator
    where
        bounds = ((0,0) , (x-1,y-1))

        indices = range bounds
        origin@(ox,oy) = getRelativePos startingPos (x,y)
        absoluteSeedLocations = fmap (bimap (+ox) (+oy)) relativeSeedLocations
        orderedIndices = indexPath origin indices \\ absoluteSeedLocations

        allColors = makeAllColors (x,y)
        (startingColors, colors) = pickStartingColors randomGenerator allColors (length absoluteSeedLocations)

        startingPixels = zip absoluteSeedLocations startingColors

        pixels = addColoredPixels startingPixels emptyScreen


makePicture
    :: ((Int,Int) , (Int,Int))
    -> [(Int, Int)]
    -> Int
    -> ColorTree
    -> Pixels
    -> ColorPicker' ColorTree Color
    -> ColorCombinator [] Color
    -> Pixels

makePicture _ _ [] _ pixels _ _
    = pixels
makePicture bounds searchDistance (index:indices) colors pixels colorPicker colorCombinator
    | null colors
        = pixels
    | otherwise
        = makePicture bounds indices updatedColors updatedPixels colorPicker colorCombinator
    where
        surroundingColors = getSurroundingColors bounds searchDistance pixels index

        combinedSurrColors = colorCombinator surroundingColors

        (pickedColor, updatedColors) = colorPicker combinedSurrColors colors

        updatedPixels = addColoredPixel index pickedColor pixels


getSurroundingColors
    :: ((Int,Int) , (Int,Int))
    -> Pixels
    -> (Int,Int)
    -> [Color]

getSurroundingColors bounds searchDist pixels (x,y)
    = [ color | Just color <- maybeColors ]
    where
        offsets = range ((-searchDist,-searchDist) , (searchDist,searchDist))

        surroundingIndices = filter (isInsidePicture bounds) . fmap (bimap (+x) (+y)) $ offsets

        maybeColors = fmap (pixels !) surroundingIndices


isInsidePicture
    :: ((Int,Int) , (Int,Int))
    -> (Int,Int)
    -> Bool

isInsidePicture ((lx,ly), (ux,uy)) (_x,_y)
    =  _x >= lx
    && _x <= ux
    && _y >= ly
    && _y <= uy