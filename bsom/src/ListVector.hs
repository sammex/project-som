module ListVector (
    Vec,
    (>+),
    (>-),
    (>*),
    absv,
    editNth
) where

-- | This type assignment is used to provide more intuitive working with vectors.
type Vec a = [a]

-- | Add two vectors. This is defined as @(>+) = zipWith (+)@.
(>+) :: Num a => Vec a -> Vec a -> Vec a
(>+) = zipWith (+)

-- | Subtract two vectors. This is defined as @(>-) = zipWith (-)@.
(>-) :: Num a => Vec a -> Vec a -> Vec a
(>-) = zipWith (-)

-- | Multiply vector by a scalar. This is defined as @(>*) x y = map (y *) x@.
(>*) :: Num a => Vec a -> a -> Vec a
(>*) x y = map (y *) x

-- | Length of vector, calculated using the euclidean distance.
absv :: Floating a => Vec a -> a
absv x = sqrt $ foldl (\s e -> s + e ^ (2 :: Int)) 0 x

editNth :: (a -> a) -> Int -> [a] -> [a]
editNth f 0 l = case l of
                     [] -> []
                     [x] -> [f x]
                     (x:xs) -> f x : xs
editNth f n l = if n <= 0 then l else case l of
                     [] -> []
                     [x] -> [x]
                     (x:xs) -> x : editNth f (n-1) xs
