module AlphaFunction (
    quadraticAlpha,
    reversedAlpha,
    linearAlpha
) where

import ListVector

-- | A epoch-based alpha distribution using quadratic functions for underlying calculations.
quadraticAlpha :: (Int -> Int -> Int)
                  -> (Int -> Double -> Vec Int -> Vec Int -> Double) -- ^ A function which takes a radius and an alpha value and
                                                                     --  returns a radius-based alpha function.
                  -> Double -- ^ A initial alpha value. It decreases quadratically, which
                            -- means it starts to decrease slowly and then faster.
                  -> Int
                  -> Int
                  -> Vec Int
                  -> Vec Int
                  -> Double
quadraticAlpha radd radf a count i = let co = a / (fromIntegral count ^ 2)
                                     in  radf (radd count i) $ co * (fromIntegral $ i - count) ^ 2

-- | This epoch-based alpha distribution is unique, because it does not depend
-- on the amount of training epochs, but rather decreases like @1/x@. The more
-- training samples there are, the more accurate are the results.
reversedAlpha :: (Int -> Int -> Int)
                 -> (Int -> Double -> Vec Int -> Vec Int -> Double)
                 -> Double
                 -> Int
                 -> Int
                 -> Vec Int
                 -> Vec Int
                 -> Double
reversedAlpha radd radf a count i vf vc = radf (radd count i) (a / (fromIntegral i + 1)) vf vc

-- | This epoch-based alpha distribution uses a linearly decreasing alpha value.
linearAlpha :: (Int -> Int -> Int)
               -> (Int -> Double -> Vec Int -> Vec Int -> Double)
               -> Double
               -> Int
               -> Int
               -> Vec Int
               -> Vec Int
               -> Double
linearAlpha radd radf a count i = radf (radd count i) ((-a) * (fromIntegral i) / (fromIntegral count) + a)
