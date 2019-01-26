module Main (main) where

import PictureOrigin
import IndexPath
import ColorCombinator
import ColorPicker
import Color
import ColorPalette
import Picture

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
    
    makeArtAndSave "Circle" bounds TheMiddle [(0,0)] 2 circularPath pickClosest maxColor


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
        max = V3 maxRed maxGreen maxBlue
        
        -- Necessary because makeImage seems to take the y coordinate first and x coordinate second
        invertCoords (x,y) = (y,x)
        
        image = makeImage (invertCoords bounds) (toDoublePixel max . (art !?) . invertCoords) :: Image VS RGB Double

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
        origin@(ox,oy) = getAbsolutePos pictureOrigin (x,y)
        absoluteSeedLocations = fmap (bimap (+ox) (+oy)) relativeSeedLocations
        orderedIndices = indexPath origin indices \\ absoluteSeedLocations

        allColors = makeAllColors (x,y)
        (startingColors, colors) = pickStartingColors randomGenerator allColors (length absoluteSeedLocations)

        startingPicture = zip absoluteSeedLocations startingColors

        picture = addPixels startingPicture emptyPicture


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

        updatedPicture = addAPixel index pickedColor picture


-- | Returns the colors surrounding the given pixel.
getSurroundingColors
    :: ((Int,Int) , (Int,Int))
    -> Int
    -> Picture
    -> (Int,Int)
    -> [Color]

getSurroundingColors bounds searchDist picture (x,y)
    = [ color | Just color <- maybeColors ]
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