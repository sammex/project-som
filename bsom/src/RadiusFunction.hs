module RadiusFunction (
    squareRadiusTime,
    squareRadiusField,
    reversedRadiusField
) where

-- | A epoch-based radius distribution using a quadratic function for underlying calculations.
squareRadiusTime :: Int -- ^ starting radius
                -> Int -> Int -> Int
squareRadiusTime start count i = let a = fromIntegral start / (fromIntegral $ count ^ 2)
                             in  round (a * (fromIntegral i - fromIntegral count) ^ 2 :: Double)

-- | A distribution of the alpha value throughout different radii. It can be
-- viewed as a graph of a parabola, whereas radii are noted on the x-axis and
-- alpha values are noted on the y-axis.
squareRadiusField :: (Double -> Int -> Int -> Double) -- ^ The final radius-based alpha distribution.
squareRadiusField alpha maxr actr = let {
            x = fromIntegral actr;
            rm = fromIntegral maxr;
            a = alpha / (rm ^ 2);
    } in if rm == 0.0 then alpha else a * (x - rm) ^ 2

-- | Like 'squareRadiusDistribution', but instead of using @x^2@, using @1/x@.
reversedRadiusField :: (Double -> Int -> Int -> Double)
reversedRadiusField alpha _ actr = let x = fromIntegral actr
                                          in  alpha / (x + 1)
