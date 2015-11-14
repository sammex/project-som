module FieldInterpretation (
    squareInterpretation,
    independentInterpretation,
    hexagonalInterpretation
) where

import ListVector

-- | A function taking an alpha value returning a radius-based alpha function, as discussed
--  with the 'updateWinner' function. @independentRadius a w t@ returns @a@ if
--  @t == w@, and @0@ if @t /= w@. In other words, only the winning prototype is
--  updated with an alpha factor of @a@.
independentInterpretation :: Double -> Vec Int -> Vec Int -> Double
independentInterpretation alpha win test = if win == test then alpha else 0.0

-- | A radius interpretation using squares as prototype "tiles".
squareInterpretation :: (Double -> Int -> Int -> Double) -> Int -> Double -> Vec Int -> Vec Int -> Double
squareInterpretation distr rad alpha win test = let distance = maximum $ zipWith (\a b -> abs $ a - b) test win
                                                in  if distance > rad then 0.0 else distr alpha rad distance

-- | A radius interpretation using hexagonal "tiles" for the prototypes.
hexagonalInterpretation :: (Double -> Int -> Int -> Double) -> Int -> Double -> Vec Int -> Vec Int -> Double
hexagonalInterpretation distr rad alpha win test = distr alpha rad (let (wx, wy) = takePair win
                                                                        (tx, ty) = takePair test
                                                                        dx = tx - wx
                                                                        dy = ty - wy
                                                                    in  if (signum dx) * (signum dy) == (-1) || (signum dx) * (signum dy) == 0 then (abs dx) + (abs dy) else max (abs dx) (abs dy))
        where {takePair [] = (0, 0); takePair (x:[]) = (x, 0); takePair (x:y:_) = (x, y)}
