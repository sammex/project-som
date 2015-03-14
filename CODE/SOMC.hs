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
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List

type Vec a = Vector.Vector a
-- some point in a coordinate system
type DataPoint = Vec Double
-- a prototype in a coordinate system
data Prototype = Prototype {point :: DataPoint, position :: Vec Int} deriving (Show)

instance Eq Prototype where
    a == b = position a == position b

-- for more intuitive typing (haha!)
type Prototypes = [Prototype]
type DataSet = Set.Set DataPoint

-- MultiDimensional Coordinate System
data MDCS = MDCS {dataPoints :: DataSet, prototypes :: [Prototype]} deriving (Show)

(-@>) :: [Double] -> [Int] -> Prototype
(-@>) poi pos = Prototype (Vector.fromList poi)  (Vector.fromList pos)

-- add two vectors (result vector has lowest dimension of both arguments)
(>+) :: Num a => Vec a -> Vec a -> Vec a
(>+) = Vector.zipWith (+)

-- subtract two vectors (result vector has lowest dimension of both arguments)
(>-) :: Num a => Vec a -> Vec a -> Vec a
(>-) = Vector.zipWith (-)

-- multiply vector by scalar
(>*) :: Num a => Vec a -> a -> Vec a
(>*) x y = Vector.map (y *) x

-- absolute of vector (also known as length)
absv :: Floating a => Vec a -> a
absv x = sqrt $ Vector.foldl (\s e -> s + e ^ (2 :: Int)) 0 x

editNth :: (a -> a) -> Int -> [a] -> [a]
editNth f 0 l = case l of
		     [] -> []
                     [x] -> [f x]
		     (x:xs) -> f x : xs
editNth f n l = if n < 0 then l else case l of
		     [] -> []
                     [x] -> [x]
		     (x:xs) -> x : editNth f (n-1) l

getWinner :: (DataPoint -> DataPoint -> Double) -> Prototypes -> DataPoint -> Prototype
getWinner distance prlist dat = Maybe.fromJust $ snd $ foldl (
    \mwinner prot -> if Maybe.isNothing (snd mwinner)
        then (distance dat $ point prot, Just prot)
        else let dist = distance dat $ point prot in if   dist < fst mwinner
                                                     then (dist, Just prot)
                                                     else mwinner
    ) (0, Nothing) prlist

updateWinner :: (Vec Int -> Vec Int -> Double) -> Prototypes -> Prototype -> DataPoint -> Prototypes
updateWinner alpha prlist wp wd = map (\x -> x {point = ((wd >- point x) >* alpha (position wp) (position x)) >+ point x}) prlist

-- updates all prototypes of a MDCS by going through every data point (distance function -> alpha function (winner prototype -> test prototype -> alpha) -> old MDCS -> new MDCS)
epoch :: (DataPoint -> DataPoint -> Double) -> (Vec Int -> Vec Int -> Double) -> MDCS -> Prototypes
epoch distance alpha mdcs = Set.foldl (
        \upr dat -> let winner = getWinner distance upr dat
                    in  updateWinner alpha upr winner dat
    ) (prototypes mdcs) (dataPoints mdcs)

sortToGroups :: (DataPoint -> DataPoint -> Double) -> MDCS -> [DataSet]
sortToGroups d (MDCS set prots) = Set.foldl (
		\datslist pnt -> editNth (Set.insert pnt) (Maybe.fromJust $ List.elemIndex (getWinner d prots pnt) prots) datslist
    ) (replicate (length prots) Set.empty) set

-- calculates the euclidean distance between two points
euclidDistance :: DataPoint -> DataPoint -> Double
euclidDistance a b = absv $ a >- b

-- if test prototye equals winner prototype then alpha, else 0
independentAlpha :: Double -> Vec Int -> Vec Int -> Double
independentAlpha alpha win test = if win == test then alpha else 0.0

printSet :: Show a => Set.Set a -> IO ()
printSet = Set.foldl (\i e -> i >> print e) (return ())

printList :: Show a => [a] -> IO ()
printList = foldl (\i e -> i >> print e) (return ())

myMDCS :: MDCS
myMDCS = MDCS (Set.fromList [Vector.singleton (-1.0), Vector.singleton 1.0, Vector.singleton 3.0, Vector.singleton 5.0]) [[0.0] -@> [0], [0.0] -@> [1], [0.0] -@> [2], [0.0] -@> [3]]

epoch10 :: MDCS
epoch10 = iterate (\mdcs -> mdcs {prototypes=epoch euclidDistance (independentAlpha 0.2) mdcs}) myMDCS !! 10

main :: IO ()
main = print epoch10
