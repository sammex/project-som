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

module SOMC (
    -- * Basic Types
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
    -- ** Processing Multi-Dimensional Coordinate Systems
    sortToGroups,
    -- ** Auxiliary Functions
    euclidDistance,
    independentAlpha,
    printSet,
    printList
) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

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

-- | Calculates the euclidean distance between two points, using the 'absv'
--  function.
euclidDistance :: DataPoint -> DataPoint -> Double
euclidDistance a b = absv $ a >- b

-- | A radius function, as discussed with the 'updateWinner' function.
--  @independentAlpha a w t@ returns @a@ if @t == w@, and @0@ if @t /= w@. In
--  other words, only the winning prototype is updated with an alpha factor of
--  @a@.
independentAlpha :: Double -> Vec Int -> Vec Int -> Double
independentAlpha alpha win test = if win == test then alpha else 0.0

-- | Prints a set in a neat way by just printing every element, so the developer
--  can see what the set contains in an easy way.
printSet :: Show a => Set a -> IO ()
printSet = Set.foldl (\i e -> i >> print e) (return ())

-- | Works the same es 'printSet', just with lists.
printList :: Show a => [a] -> IO ()
printList = foldl (\i e -> i >> print e) (return ())

myMDCS :: (DataSet, Prototypes)
myMDCS = ((Set.fromList [[(-1.0)], [1.0], [3.0], [5.0]]), [[0.0] -@> [0], [0.0] -@> [1], [0.0] -@> [2], [0.0] -@> [3]])

epoch10 :: MDCS
epoch10 = iterate (\mdcs@(ds, _) -> (ds, epoch euclidDistance (independentAlpha 0.2) mdcs)) myMDCS !! 10

main :: IO ()
main = print epoch10
