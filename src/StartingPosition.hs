module StartingPosition where


data StartingPos
    = TopLeft
    | TopMiddle
    | TopRight
    | MiddleLeft
    | TheMiddle
    | MiddleRight
    | BottomLeft
    | BottomMiddle
    | BottomRight
    | StartingPos Double Double
    deriving (Show)


getAbsolutePos :: StartingPos -> (Double, Double)
getAbsolutePos TopLeft           = (0  , 1  )
getAbsolutePos TopMiddle         = (0.5, 1  )
getAbsolutePos TopRight          = (1  , 1  )
getAbsolutePos MiddleLeft        = (0  , 0.5)
getAbsolutePos TheMiddle         = (0.5, 0.5)
getAbsolutePos MiddleRight       = (1  , 0.5)
getAbsolutePos BottomLeft        = (0  , 0  )
getAbsolutePos BottomMiddle      = (0  , 0.5)
getAbsolutePos BottomRight       = (0  , 1  )
getAbsolutePos (StartingPos x y) = (x  , y  )


getRelativePos :: StartingPos -> (Int, Int) -> (Int, Int)
getRelativePos startingPos (x,y)
    = (round (fromIntegral x * aX), round (fromIntegral y * aY))
    where (aX,aY) = getAbsolutePos startingPos
