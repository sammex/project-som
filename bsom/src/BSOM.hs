-- This is a file of "project-somc". The goal of the project is to make a
-- self-organizing memory card to group data, i.e. for making book
-- recommendations. Copyright (C) 2015 Julius Quasebarth
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program. If not, see http://www.gnu.org/licenses/.
--
-- -Julius Quasebarth (feldrandstudios@gmail.com)

{-|
Module : SOMC
Description : Used for sorting data into groups, preprocessing data.
Copyright : (c) Julius Quasebarth, Robin Hankel, Luisa Derer 2015
License : GPL-3
Maintainer : feldrandstudios@gmail.com
Stability : experimental
Portability : No restrictions

This package exports functions to group and manipulate points in
multi-dimensional space using __s__elf-__o__rganizing __m__emory __c__ards.

To understand the mechanics of most functions, it is necessary to understand how
a self-organizing memory card works, at least how it works in this project.

== Understanding Self-Organizing Memory Cards
=== What you want to do, and what you need for it
You want to group data. For this reason, you obviously need two components:
Data, and you have to know how many groups you want.

But before you can group your data, you need a list of prototypes. Prototypes
are essential to grouping data with memory cards, because they are the
representation of groups. Thus you need as many prototypes as groups.
Furthermore, prototypes contain a representation point in \"data space\" (space
for data points) and a prototype position in \"prototype space\". It is a
convention to initialize the data space with random values, while the prototypes
are aligned tidily in prototype space.

=== Getting to the point
Because a self-organizing memory card is a type of neural network, it works by
training. It is trained by going through every data point in your data set and
moving the prototype closest to the data point a bit more in that direction.
There is a problem with that: The resulting groups (prototypes) are not ordered
at all. You can fix that by doing the following: If you move a prototype (lets
call it \"the winner\"), you move all prototypes near the winner slightly to the
direction of the point the winner moves to. And with \"near\" I mean close in
another space, the /prototype space/! In the prototype space, all prototypes are
aligned on a grid. Using this technique, prototypes next to each other in
prototype space will always be next to each other in data space, too.
-}

module Main (
    main,
    -- * Basic Types
    Vec,
    DataPoint,
    DataSet,
    Prototype,
    (-@>),
    Prototypes,
    MDCS,
    -- * Functions for working with Vectors
    (>+),
    (>-),
    (>*),
    absv,
    -- * Working with Multi-Dimensional Coordinate Systems
    -- ** Using Self-Organizing Memory Cards
    getWinner,
    updateWinner,
    epoch,
    train,
    -- ** Processing Multi-Dimensional Coordinate Systems
    sortToGroups,
    -- ** Auxiliary Functions
    -- $build
    printSet,
    printList,
    euclidDistance,
    -- *** Base Alpha Functions
    quadraticAlpha,
    reversedAlpha,
    linearAlpha,
    -- *** Prototype Space Interpretations
    squareInterpretation,
    independentInterpretation,
    hexagonalInterpretation,
    -- *** Epoch-Based Radius Functions
    squareRadius,
    -- *** Radius-Based Alpha Functions
    squareRadiusDistribution
) where

import Control.Monad
import Control.DeepSeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Debug.Trace
import qualified Data.Foldable as F
import System.Environment
import System.IO
import System.Random

-- | This type assignment is used to provide more intuitive working with vectors.
type Vec a = [a]
-- | A point in multi-dimensional space.
type DataPoint = Vec Double
-- | A prototype in multi-dimensional space. It both uses a \"represenation
--  point\" and a \"position in prototype space\". The \"representation point\"
--  is (or should be) the middle of a group of data points, whereas the position
--  in prototype space is used for regularly distributing representation points
--  in data point space. This is done by moving prototypes close to each other
--  in prototype space near to each other in data point space.
data Prototype = Prototype {
    -- | The representation point in data point space.
    point :: DataPoint,
    -- | The prototype position in prototype space. It is integer because
    --  prototypes are distributed evenly.
    position :: Vec Int
} deriving (Show)

-- | Two prototypes are equal if their position in prototype space is equal.
instance Eq Prototype where
    a == b = position a == position b

instance NFData Prototype where
    rnf (Prototype pnt pos) = rnf pnt `seq` rnf pos

-- | This type assignment is used for more intuitive typing @:)@.
type Prototypes = [Prototype]
-- | This type assignment is used for more intuitive typing, too.
type DataSet = Set DataPoint
-- | __M__ulti-__D__imensional __C__oordinate __S__ystem.
--  This type assignment is used to combine both data of the data space and
--  prototypes of prototype space.
type MDCS = (DataSet, Prototypes)

-- | The prototype assignment operator just says \"clip this data point to that
--  prototype position\".
(-@>) :: [Double] -> [Int] -> Prototype
(-@>) poi pos = Prototype poi pos

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

-- | Returns the prototype for which the given function returns the lowest value.
getWinner :: (DataPoint -> DataPoint -> Double) -- ^ A function which takes the
                                                --  position of the data point
                                                --  and the representation point
                                                --  of a prototype and returns
                                                --  the distance of both
                                                --  (\"distance function\").
             -> Prototypes -- ^ The prototypes to choose from.
             -> DataPoint -- ^ The data point to compare with.
             -> Prototype -- ^ The \"winning\" prototype.
getWinner distance prlist dat = Maybe.fromJust $ snd $ foldl (
    \mwinner prot -> if Maybe.isNothing (snd mwinner)
        then (distance dat $ point prot, Just prot)
        else let dist = distance dat $ point prot in if   dist < fst mwinner
                                                     then (dist, Just prot)
                                                     else mwinner
    ) (0, Nothing) prlist

-- | Updates a prototype given a \"radius function\" (a distance function for
--  prototype space), a list of available prototypes, the \"winner\" prototype
--  and a data point. Essentially, this function first generates a vector from
--  any prototype to the given data point and then moves the prototype by a
--  fraction of that vector, whereas the fraction is given by the radius
--  function.
updateWinner :: (Vec Int -> Vec Int -> Double) -- ^ The radius function. It
                                               --  takes the position of the
                                               --  winner prototype and the
                                               --  position of any prototype in
                                               --  prototype space and returns
                                               --  how much (relative) the
                                               --  prototype should be moved.
                -> Prototypes -- ^ The prototype list to choose from.
                -> Prototype -- ^ The winning prototype.
                -> DataPoint -- ^ The data point to in which direction the
                             --  prototypes should be moved.
                -> Prototypes -- ^ The modified prototype list.
updateWinner alpha prlist wp wd = map (\x -> x {point = ((wd >- point x) >* alpha (position wp) (position x)) >+ point x}) prlist

-- | Trains the self-organizing memory card in one \"epoch\". An epoch is a
--  training cycle, so for every given data point, the nearest prototype is
--  determined (using the 'getWinner' function) and then all prototypes are
--  updated (based on the 'updateWinner' function).
epoch :: (DataPoint -> DataPoint -> Double) -- ^ The distance function, as
                                            --  discussed with the 'getWinner'
                                            --  function.
         -> (Vec Int -> Vec Int -> Double) -- ^ The radius function, as
                                           --  discussed with the 'updateWinner'
                                           --  function.
         -> MDCS -- ^ The MDCS containing data points and prototypes.
         -> Prototypes -- ^ The trained prototype list.
epoch distance alpha mdcs = Set.foldl (
        \upr dat -> let winner = getWinner distance upr dat
                    in  updateWinner alpha upr winner dat
    ) (snd mdcs) (fst mdcs)

-- | This function sorts the data points of a MDCS to groups by going through
--  every data point in the 'DataSet' and then assigning it to the prototype
--  which representation point is the closest to the data point.
sortToGroups :: (DataPoint -> DataPoint -> Double) -- ^ The distance function,
                                                   --  as discussed with the
                                                   --  'getWinner' function.
                -> MDCS -- ^ The MDCS containing data points to sort and
                        --  prototypes to sort to.
                -> [(Prototype, DataSet)] -- ^ The data points of the data set
                                          --  of the MDCS, assigned to the
                                          --  protoypes.
sortToGroups d (set, prots) = Set.foldl (
        \rl pnt ->
        let winner = getWinner d prots pnt
        in  map (
            \(prttt, nset) ->
            if   prttt == winner
            then (prttt, Set.insert pnt nset)
            else (prttt, nset)
        ) rl
    ) (zip prots $ repeat Set.empty) set

-- | A function for training a SOM. It iterates the epoch-function.
train :: (DataPoint -> DataPoint -> Double) -- ^ distance function
         -> (Int -> Int -> Vec Int -> Vec Int -> Double) -- ^ Radius function, enhanced by two arguments: The first
                                                         -- one is the amount of training cycles, the second is the
                                                         -- current training cycle (as the output of this function
                                                         -- should change over time).
         -> MDCS
         -> Int -- ^ amount of training cycles
         -> Prototypes
train dist prad (dat, prot) count =
    let (fp, _) = iterate (\(protm, i) -> (epoch dist (prad count i) (dat, protm), i+1)) (prot, 1) !! count
    in fp

-- $build
-- The following functions can be used to build customized training functions.
-- It is important to understand how they work together, so they can be built
-- together correctly. 'train' and 'epoch' both use alpha-functions which
-- tell how far a prototype has to be moved. As the names of 'quadraticAlpha',
-- 'reversedAlpha' and 'linearAlpha' suggest, these are the most important
-- parts of configuration. Their types are equal. They all take a radius function
-- , a radius-based alpha function and a initial alpha value and return a
-- configured alpha function. Radius functions are constructed using 'squareRadius'
-- or similar functions. Radius-based alpha functions are constructed using
-- an interpretation, for example 'squareInterpretation'. But an interpretation
-- also needs to know how the alpha value should change with greater radius, so
-- it needs a radius-based alpha distribution. So a complete alpha function could
-- look like @quadraticAlpha (squareRadius 3) (hexagonalInterpretation
-- squareRadiusDistribution) 0.3@.

-- | Calculates the euclidean distance between two points, using the 'absv'
--  function.
euclidDistance :: DataPoint -> DataPoint -> Double
euclidDistance a b = absv $ a >- b

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

-- | A epoch-based radius distribution using a quadratic function for underlying calculations.
squareRadius :: Int -- ^ starting radius
                -> Int -> Int -> Int
squareRadius start count i = let a = fromIntegral start / (fromIntegral $ count ^ 2)
                             in  round (a * (fromIntegral i - fromIntegral count) ^ 2 :: Double)

-- | A distribution of the alpha value throughout different radii. It can be
-- viewed as a graph of a parabola, whereas radii are noted on the x-axis and
-- alpha values are noted on the y-axis.
squareRadiusDistribution :: (Double -> Int -> Int -> Double) -- ^ The final radius-based alpha distribution.
squareRadiusDistribution alpha maxr actr = let x = fromIntegral actr
                                               rm = fromIntegral maxr
                                               a = alpha / (rm ^ 2)
                                           in  if rm == 0.0 then alpha else a * (x - rm) ^ 2

reversedRadiusDistribution :: (Double -> Int -> Int -> Double)
reversedRadiusDistribution alpha _ actr = let x = fromIntegral actr
                                          in  alpha / (x + 1)

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

randomMDCS :: Int -- ^ square root of prototypes
              -> Int -- ^ number of data points
              -> Double -- ^ maximum x value (data points)
              -> Double -- ^ maximum y value (data points)
              -> IO MDCS
randomMDCS prn dan maxx maxy = do {
    preprts <- replicateM (prn ^ 2) $ do {x <- randomRIO (0.2 * maxx, 0.8 * maxx); y <- randomRIO (0.2 * maxy, 0.8 * maxy); return [x, y]}; -- preprts :: [[Double]]
    let {prts = fst $ foldl (\(l, (x, y)) e -> ((e -@> [x, y]) : l, (if x == prn - 1 then (0, y + 1) else (x + 1, y)))) ([], (0, 0)) preprts};
    dats <- replicateM dan $ do {x <- randomRIO (0, maxx); y <- randomRIO (0, maxy); return [x, y]};
    return (Set.fromList dats, prts);
    }

-- | Prints a set in a neat way by just printing every element, so the developer
--  can see what the set contains in an easy way.
printSet :: Show a => Set a -> IO ()
printSet = Set.foldl (\i e -> i >> print e) (return ())

-- | Works the same es 'printSet', just with lists.
printList :: Show a => [a] -> IO ()
printList = foldl (\i e -> i >> print e) (return ())

myMDCS :: (DataSet, Prototypes)
myMDCS = ((Set.fromList [[(-1.0)], [4.5], [4.0], [5.0], [(-1.3)], [(-2.0)], [10.0], [10.5], [14.0], [14.01], [14.6], [15.0]]), [[-1] -@> [0], [2] -@> [1], [8] -@> [2], [12] -@> [3]])

myMDCS2 = ((Set.fromList [
        [3, 2],
        [3.2, 2.5],
        [4, 2],
        [4, 3],
        [2.2, 7.2],
        [3, 8],
        [3.1, 7.1],
        [4, 8],
        [2.5, 15.5],
        [2.7, 16],
        [9, 2.1],
        [10, 2],
        [9.8, 3.1],
        [10.5, 3],
        [7, 6],
        [7, 7],
        [7.5, 6.5],
        [8, 6],
        [6.8, 9.5],
        [7, 10],
        [6.4, 10.2],
        [7.2, 9.5],
        [7.5, 10.5],
        [9.5, 14.9],
        [9.6, 15.2],
        [10, 15.9],
        [10.1, 15],
        [10.2, 15.2],
        [10.8, 15.7],
        [10.5, 14.5],
        [11.2, 15.5]
    ]), [[3, 4.5] -@> [0], [2, 12] -@> [1], [7, 5] -@> [2], [5, 10] -@> [3], [5.5, 16] -@> [4], [8.5, 14.5] -@> [5], [11, 8] -@> [5]])

randomColors :: Int -> IO [RGB Word8]
randomColors n = let hrange = 360.0 / fromIntegral n :: Double
                 in  getStdGen >>=
                     \stdg -> return $ map (fmap (truncate . (* 255))) $ (\(x, _, _, _) -> x) $ flip (!!) n $ iterate (
                         \(l, b, curN, gen) -> let (newHue, gen2) = randomR (hrange * fromIntegral curN, hrange * (fromIntegral curN + 1)) gen
                                                   (newVal, gen3) = randomR (if b then (0.7, 0.8) else (0.3, 0.4)) gen2
                                               in  (hsv newHue 1.0 newVal : l, not b, curN + 1, gen3)
                     ) ([], True, 0, stdg)

plotRandomColors :: [RGB Word8] -> Image PixelRGB8
plotRandomColors cl = generateImage (\x y -> uncurryRGB PixelRGB8 (cl !! x)) (length cl) 1

plotSquareRadius :: Int -- ^ starting radius
                    -> Int -- ^ amount of training samples
                    -> Image Pixel8
plotSquareRadius r c = let vals = [squareRadius r c x | x <- [0..c]]
                           m = maximum vals
                       in  generateImage (\x y -> if squareRadius r c x == m - y then 0 else 255) c m

plotSquareRadiusDistribution :: Double -- ^ squareRadiusDistribution argument: Alpha value
                                -> Int -- ^ maximum radius
                                -> Int -- ^ scale by
                                -> Image Pixel8
plotSquareRadiusDistribution a rm sc = let vals = [squareRadiusDistribution a rm x | x <- [0..rm]]
                                           m = round $ fromIntegral sc * maximum vals
                                       in  generateImage (\x y -> if round ((fromIntegral sc) * squareRadiusDistribution a rm x) == m - y then 0 else 255) rm m

plotAlphaFunction :: ((Int -> Int -> Int) -> (Int -> Double -> Vec Int -> Vec Int -> Double) -> Double -> Int -> Int -> Vec Int -> Vec Int -> Double)
                     -> Double -- ^ alpha value
                     -> Int -- ^ amount of training samples
                     -> Int -- ^ scale
                     -> Image Pixel8
plotAlphaFunction alph val cntt sc = let fakeRadius = \_ _ -> 0
                                         fakeAlpha = \_ x _ _ -> x
                                         scale = fromIntegral sc
                                         baseAlpha = \i -> scale * alph fakeRadius fakeAlpha val cntt i [0] [0]
                                     in  generateImage (\x y -> if (round $ baseAlpha x) == sc - y then 0 else 255) cntt sc

plot2DMDCS :: MDCS
              -> Int -- ^ scale
              -> Image Pixel8
plot2DMDCS (dset, prots) sc = let dlist = Set.toList dset
                                  maxx = maximum (map (flip (!!) 0) dlist)
                                  maxy = maximum (map (flip (!!) 1) dlist)
                                  xrange = maxx
                                  yrange = maxy
                                  scale = fromIntegral sc
                              in  generateImage (
                                      \x y -> if any (isPointAtPixel scale x y) (map point prots) then 100 else
                                          if any (isPointAtPixel scale x y) dlist then 0 else 255
                                  ) (sc * (round xrange)) (sc * (round yrange))

plotPrototypes :: Prototypes
                  -> Int -- ^ scale
                  -> Int -- ^ maxx
                  -> Int -- ^ maxy
                  -> Image Pixel8
plotPrototypes l sc mx my = let scale = fromIntegral sc
                                pointList = map (map (round . (*) scale) . point) l
                            in  generateImage (\x y -> if elem [x, y] pointList then 0 else 255) (sc * mx) (sc * my)

plot2DMDCSGroups :: [(Prototype, DataSet)]
                    -> Int -- ^ scale
                    -> [RGB Word8] -- ^ colors
                    -> Image PixelRGB8
plot2DMDCSGroups l sc groupColors = let colorGroups = zip groupColors $ map snd l
                                        dlist = concatMap (\(p, ds) -> Set.toList ds) l
                                        maxx = maximum (map (flip (!!) 0) dlist)
                                        maxy = maximum (map (flip (!!) 1) dlist)
                                        xrange = maxx
                                        yrange = maxy
                                        scale = fromIntegral sc
                                    in  generateImage
                                            (\x y -> uncurryRGB PixelRGB8 $ pixelGroupColor scale x y colorGroups)
                                            (sc * (round xrange)) (sc * (round yrange))

isPointAtPixel :: Double -- ^ scale
                  -> Int -- ^ possible coordinate x
                  -> Int -- ^ possible coordinate y
                  -> [Double] -- ^ data point (2D)
                  -> Bool -- ^ data point is at possible coordinate?
isPointAtPixel scale x y l = x == (round $ (*) scale $ l !! 0) && y == (round $ (*) scale $ l !! 1)

pixelGroupColor :: Double -- ^ scale
                   -> Int -- ^ possible coordinate x
                   -> Int -- ^ possible coordinate y
                   -> [(RGB Word8, DataSet)]
                   -> RGB Word8
pixelGroupColor scale x y l = let fl = filter (
                                           \(rgb, ds) -> any (isPointAtPixel scale x y) ds
                                       ) $ map (\(rgb, ds) -> (rgb, Set.toList ds)) l
                              in  if fl == [] then RGB 0 0 0 else fst $ head fl

main :: IO ()
main = do {
    let {gCount = 7};
    let {pCount = 1000};
    let {scale = 50};
    arg <- fmap head getArgs;
    putStrLn "Generating MDCS...";
    rmdcs <- randomMDCS gCount pCount 10.0 10.0;
    putStrLn "Plotting random MDCS...";
    rplot <- return $ force $ plot2DMDCS rmdcs scale;
    putStrLn "Saving random MDCS...";
    savePngImage (arg ++ "/origMDCS.png") $ ImageY8 rplot;
    putStrLn "Generating Colors...";
    colrs <- randomColors $ gCount ^ 2;
    putStrLn "Saving Colors...";
    savePngImage (arg ++ "/colorMap.png") $ ImageRGB8 $ plotRandomColors colrs;
    putStrLn "Saving Prototype Plot...";
    savePngImage (arg ++ "/origProts.png") $ ImageY8 $ plotPrototypes (snd rmdcs) scale 10 10;
    putStrLn "Grouping random MDCS...";
    grmdcs <- return $ force $ sortToGroups euclidDistance rmdcs;
    putStrLn "Plotting original groups...";
    savePngImage (arg ++ "/origMDCSGroups.png") $ ImageRGB8 $ plot2DMDCSGroups grmdcs scale colrs;
    putStrLn "Sorting Points...";
    trmdcs <- return $ force $ sortToGroups euclidDistance (fst rmdcs, train euclidDistance (reversedAlpha (squareRadius 3) (squareInterpretation reversedRadiusDistribution) 0.3) rmdcs 100);
    putStrLn "Plotting Points...";
    trmdcsPlot <- return $ force $ plot2DMDCSGroups trmdcs scale colrs;
    putStrLn "Saving Plot...";
    savePngImage (arg ++ "/finalMDCSGroups.png") $ ImageRGB8 trmdcsPlot;
    putStrLn "Plotting Prototypes...";
    protPlot <- return $ force $ plotPrototypes (map fst trmdcs) scale 10 10;
    putStrLn "Saving Prototype Plot...";
    savePngImage (arg ++ "/finalProts.png") $ ImageY8 protPlot;
    putStrLn "Saving Prototype Information...";
    writeFile (arg ++ "/prots.txt") $ unlines $ map (\(_, dat) -> show $ Set.size dat) trmdcs
    }
