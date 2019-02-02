module PictureOrigin where


-- | The origin from which the picture will be created. Namely IndexPath will treat this point as origin.
data PictureOrigin
    = TopLeft
    | TopMiddle
    | TopRight
    | MiddleLeft
    | Center
    | MiddleRight
    | BottomLeft
    | BottomMiddle
    | BottomRight
    | PictureOrigin Double Double
    deriving (Show)


-- | Converts the origin into a pair of doubles in the range [0,1] representing its position
getRelativePos :: PictureOrigin -> (Double, Double)
getRelativePos TopLeft             = (1  , 0  )
getRelativePos TopMiddle           = (1  , 0.5)
getRelativePos TopRight            = (1  , 1  )
getRelativePos MiddleLeft          = (0.5, 0  )
getRelativePos Center              = (0.5, 0.5)
getRelativePos MiddleRight         = (0.5, 1  )
getRelativePos BottomLeft          = (0  , 0  )
getRelativePos BottomMiddle        = (0  , 0.5)
getRelativePos BottomRight         = (0  , 1  )
getRelativePos (PictureOrigin y x) = (y  , x  )


-- | Converts the origin into an exact point in rectangle bounded by (0,0) and (y,x)
getAbsolutePos :: PictureOrigin -> (Int, Int) -> (Int, Int)
getAbsolutePos pictureOrigin (y, x)
    = (round (fromIntegral y * aY), round (fromIntegral x * aX))
    where (aY,aX) = getRelativePos pictureOrigin
