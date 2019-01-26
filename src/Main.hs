module Main (main, makeArtAndSave) where

import Color
import ColorCombinator
import ColorPalette
import ColorPicker
import IndexPath
import Picture
import PictureOrigin

import Data.Bifunctor
import Data.Ix
import Data.List

import Graphics.Image hiding (maximum)
import Linear.V3
import System.Random


main :: IO ()
main = do
    let bounds = (600, 400)
        -- bounds = (1280, 720)
        -- bounds = (1920, 1080)
    
    makeArtAndSave "Rounded square" bounds Center [(0,0)] 2 roundedSquarePath pickClosest brightestColor


-- | Creates the picture and saves it into a PNG in an art/ folder.
makeArtAndSave
    :: String
    -> (Int, Int)
    -> PictureOrigin
    -> [(Int, Int)]
    -> Int
    -> IndexPath [] (Int, Int)
    -> ColorPicker' ColorPalette Color
    -> ColorCombinator [] Color
    -> IO ()

makeArtAndSave fileName bounds pictureOrigin relativeSeedLocations searchDistance indexPath colorPicker colorCombinator
    = do
    randomGenerator <- getStdGen

    let art = makeArt bounds randomGenerator pictureOrigin relativeSeedLocations searchDistance indexPath colorPicker colorCombinator
        
        maxRed = maximum . fmap getRed $ art
        maxGreen = maximum . fmap getGreen $ art
        maxBlue = maximum . fmap getBlue $ art
        maxColor = V3 maxRed maxGreen maxBlue
        
        -- Necessary because makeImage seems to take the y coordinate first and x coordinate second
        invertCoords (x,y) = (y,x)
        
        -- Converts the picture into an image from the Graphics.Image library
        image = makeImage (invertCoords bounds) (toDoublePixel maxColor . (art !?) . invertCoords) :: Image VS RGB Double

    writeImageExact PNG [] ("art/" ++ fileName ++ ".png") image

    putStrLn ("Finished " ++ fileName ++ "!")


-- | Does few preliminary calculations (initializing few data structures and such) before giving
-- control to `makePicture` to make the picture and then returns it.
makeArt
    :: RandomGen gen
    => (Int, Int)
    -> gen
    -> PictureOrigin
    -> [(Int, Int)]
    -> Int
    -> IndexPath [] (Int, Int)
    -> ColorPicker' ColorPalette Color
    -> ColorCombinator [] Color
    -> Picture

makeArt (x,y) randomGenerator pictureOrigin relativeSeedLocations searchDistance indexPath colorPicker colorCombinator
    = makePicture bounds searchDistance orderedIndices colors picture colorPicker colorCombinator
    where
        bounds = ((0,0) , (x-1,y-1))

        indices = range bounds
        origin@(oX,oY) = getAbsolutePos pictureOrigin (x,y)
        absoluteSeedLocations = fmap (bimap (+oX) (+oY)) relativeSeedLocations
        orderedIndices = indexPath origin indices \\ absoluteSeedLocations

        allColors = makeAllColors (x,y)
        (startingColors, colors) = pickStartingColors randomGenerator allColors (length absoluteSeedLocations)

        startingPicture = zip absoluteSeedLocations startingColors

        picture = colorPixels startingPicture emptyPicture


-- | Actually draws the picture
makePicture
    :: ((Int,Int) , (Int,Int))
    -> Int
    -> [(Int, Int)]
    -> ColorPalette
    -> Picture
    -> ColorPicker' ColorPalette Color
    -> ColorCombinator [] Color
    -> Picture

makePicture _ _ [] _ picture _ _
    = picture
makePicture bounds searchDistance (index:indices) colors picture colorPicker colorCombinator
    | null colors
        = picture
    | otherwise
        = makePicture bounds searchDistance indices updatedColors updatedPicture colorPicker colorCombinator
    where
        surroundingColors = getSurroundingColors bounds searchDistance picture index

        combinedSurrColors = colorCombinator surroundingColors

        (pickedColor, updatedColors) = colorPicker combinedSurrColors colors

        updatedPicture = colorAPixel index pickedColor picture


-- | Returns the colors surrounding the given pixel.
getSurroundingColors
    :: ((Int,Int) , (Int,Int))
    -> Int
    -> Picture
    -> (Int,Int)
    -> [Color]

getSurroundingColors bounds searchDist picture (x,y)
    = [ color | Just color <- maybeColors ]  -- This removes any `Nothing`s from the maybeColors list and returns colors without `Just`s
    where
        offsets = range ((-searchDist,-searchDist) , (searchDist,searchDist))

        surroundingIndices = filter (isInsidePicture bounds) . fmap (bimap (+x) (+y)) $ offsets

        maybeColors = fmap (picture !) surroundingIndices


-- | Tells you if the given pixel is within bounds of the picture.
isInsidePicture
    :: ((Int,Int) , (Int,Int))
    -> (Int,Int)
    -> Bool

isInsidePicture ((lx,ly), (ux,uy)) (_x,_y)
    =  _x >= lx
    && _x <= ux
    && _y >= ly
    && _y <= uy