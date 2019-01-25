module Main (main) where

import StartingPosition
import IndexPath
import ColorCombinator
import ColorPicker
import Color
import ColorTree

import Data.Ix
import Data.Bifunctor
import Data.List

import Linear.V3
import System.Random
import Data.Array
import Graphics.Image hiding (Array, maximum)


-- TODO: Make starting locations configurable as an argument of makeArtAndSave
main :: IO ()
main = do
    let -- bounds = (160, 200)
        -- bounds = (200, 300)
        bounds = (400, 600)
        -- bounds = (1080, 1920)
    
    makeArtAndSave "Test Rounded Square" bounds TheMiddle roundedSquarePath pickClosestCT maxColor


makeArtAndSave
    :: String
    -> (Int, Int)
    -> StartingPos
    -> IndexPath [] (Int, Int)
    -> ColorPicker' ColorTree Color
    -> ColorCombinator [] Color
    -> IO ()

makeArtAndSave fileName bounds startingPos indexPath colorPicker colorCombinator
    = do
    randomGenerator <- getStdGen

    let art = makeArt bounds randomGenerator startingPos indexPath colorPicker colorCombinator
        
        maxRed = maximum . fmap getRed $ art
        maxGreen = maximum . fmap getGreen $ art
        maxBlue = maximum . fmap getBlue $ art
        max = V3 maxRed maxGreen maxBlue
        
        image = makeImage bounds (toDoublePixel max . (art !)) :: Image VS RGB Double

    writeImageExact PNG [] ("art/" ++ fileName ++ ".png") image

    putStrLn ("Finished " ++ fileName ++ "!")



makeArt
    :: RandomGen gen
    => (Int, Int)
    -> gen
    -> StartingPos
    -> IndexPath [] (Int, Int)
    -> ColorPicker' ColorTree Color
    -> ColorCombinator [] Color
    -> Array (Int, Int) Color

makeArt (x,y) randomGenerator startingPos indexPath colorPicker colorCombinator
    = filledInPicture
    where
        bounds = ((0,0) , (x-1,y-1))

        indices = range bounds
        origin@(ox,oy) = getRelativePos startingPos (x,y)
        startingLocations = fmap (bimap (+ox) (+oy)) [(0,0)] -- [(0,2) , (2,0) , ((-2),0) , (0,(-2))]
        orderedIndices = indexPath origin indices \\ startingLocations

        allColors = makeAllColors (x,y)
        (startingColors, colors) = pickStartingColors randomGenerator allColors (length startingLocations)

        startingPixels = zip startingLocations . fmap (Just) $ startingColors

        pixels = array bounds $ [ (xy, Nothing) | xy <- (indices \\ startingLocations) ] ++ startingPixels

        picture = makePicture bounds orderedIndices colors pixels colorPicker colorCombinator
        filledInPicture = fmap (maybe 200 id) picture


makePicture
    :: ((Int,Int) , (Int,Int))
    -> [(Int, Int)]
    -> ColorTree
    -> Array (Int, Int) (Maybe Color)
    -> ColorPicker' ColorTree Color
    -> ColorCombinator [] Color
    -> Array (Int, Int) (Maybe Color)

makePicture _ [] _ pixels _ _
    = pixels
makePicture bounds (index:indices) colors pixels colorPicker colorCombinator
    | null colors
        = pixels
    | otherwise
        = makePicture bounds indices updatedColors updatedPixels colorPicker colorCombinator
    where
        surroundingColors = getSurroundingColors bounds pixels index

        combinedSurrColors = colorCombinator surroundingColors

        (pickedColor, updatedColors) = colorPicker combinedSurrColors colors

        updatedPixels = pixels // [(index , Just pickedColor)]


getSurroundingColors
    :: ((Int,Int) , (Int,Int))
    -> Array (Int,Int) (Maybe Color)
    -> (Int,Int)
    -> [Color]

getSurroundingColors bounds pixels (x,y)
    = [ color | Just color <- maybeColors ]
    where
        offsets = range ((-2,-2) , (2,2))

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