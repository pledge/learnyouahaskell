import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


-- selective import: import Data.List (nub, sort)
-- excluding import: import Data.List hiding (nub)
-- qualified import: import qualified Data.Map
-- import qualified Data.Map as M

-- Data.List
dottedMonkey = intersperse '.' "MONKEY"
heyGuys = intercalate " " ["hey","there","guys"]
--foldl' and fold1' are non-lazy versions of foldl/r
flattenedList = concat ["foo","bar","car"] -- only on level is flattened
mapThenConcat = concatMap (replicate 4) [1..3]

allTrue = and $ map (>4) [5,6,7,8]
notAllTrue = and $ map (==4) [4,4,4,3,4]

anyTrue = or $ map (==4) [2,3,4,5,6,1]
noneTrue = or $map  (>4) [4,4,4,3,4]

anyTrue' = any (==4) [2,3,5,6,1,4]
allTrue' = all (>4) [6,9,10]

repeated10times = take 10 $ iterate (*2) 1
heyManTuple = splitAt 3 "heyman"

firstThreeElements = takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
fromTheFirstSpaceToTheEnd = dropWhile (/=' ') "This is a sentence"

-- span creates a tuple
-- first element is result of a 'takeWhile'
-- second is the remainder
(fw,rest) = span (/=' ') "This is a sentence"

(oneTwoThree, otherFour) = break (==4) [1,2,3,4,5,6,7]

sortedList = sort [8,5,3,2,1,6,4,2]
groupedList = group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
