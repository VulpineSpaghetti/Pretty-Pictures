module Color where

import Data.Ratio

import Graphics.Image
import Linear.V3


-- | `Color` defines the color of a pixel
type Color = V3 Int

-- | These functions return the red/green/blue component of the color
getRed, getGreen, getBlue :: Color -> Int
getRed   (V3 r _ _) = r
getGreen (V3 _ g _) = g
getBlue  (V3 _ _ b) = b

-- | Converts 3 ints into a color
toColor :: (Int,Int,Int) -> Color
toColor (r,g,b) = V3 r g b


-- Few typeclass instances that are necessary for the generic functions that use Color
instance (Integral n) => Real (V3 n) where
    toRational (V3 x y z) = (toInteger x + toInteger y + toInteger z) % 3


instance (Integral n) => Enum (V3 n) where
    toEnum x = V3 x' x' x'
        where x' = fromIntegral x

    fromEnum (V3 x y z) = fromIntegral (toInteger x + toInteger y + toInteger z `div` 3)


instance (Integral n) => Integral (V3 n) where
    quotRem (V3 x y z) (V3 a b c) = ( V3 (x `quot` a) (y `quot` b) (z `quot` c)
                                    , V3 (x `rem` a) (y `rem` b) (z `rem` c)
                                    )

    toInteger (V3 x y z) = (toInteger x + toInteger y + toInteger z) `div` 3


-- | Converts a color to a color with brightness relative to the maximal/brightest color
-- and converts it into a Pixel
toDoublePixel :: Color -> Color -> Pixel RGB Double
toDoublePixel max color = PixelRGB r' g' b'
    where
        (V3 r' g' b') = fmap fromIntegral color / fmap fromIntegral max


-- | Returns the distance of two colors
-- 
-- The distance is the sum of absolute differences of each component of the colors
colorDist :: (Integral n) => V3 n -> V3 n -> Int
colorDist (V3 r1 g1 b1) (V3 r2 g2 b2)
    = fromIntegral $ abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2)


-- | Returns the distance of two colors
-- 
-- The distance is the sum of squared differences of each component of the colors
colorDistSq :: (Integral n) => V3 n -> V3 n -> Int
colorDistSq c1 c2
    = fromIntegral . fmap ((^2) . fromIntegral) $ c1 - c2