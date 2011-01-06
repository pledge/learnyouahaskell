multThree :: (Num a) => a -> a -> a ->a
multThree x y z = x * y * z

multTwoWithNine = multThree 9
fiftyFour = multTwoWithNine 2 3

multWithEighteen = multTwoWithNine 2
oneHundredEighty = multWithEighteen 10

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- equivalent
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- must use subtract as - is a conveinience for minus

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

sixteen = applyTwice (+3) 10
listOfThreeThreeOne = applyTwice (3:) [1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

sums = zipWith (+) [4,2,5,6] [2,6,2,3]
maxes = zipWith' max [6,3,2,1] [7,3,1,5]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
	where g x y = f y x
-- equivalent
flip'' f y x  = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

withThreeAdded = map' (+3) [1,5,3,1,6]


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
	| p x        = x : filter p xs
	| otherwise  = filter p xs

onlyGtThree = filter' (>3) [1,5,3,2,1,6,4,3,2,1]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
	let smallerSorted = quicksort (filter (<=x) xs)
	    biggerSorted = quicksort (filter (>x) xs)
	in smallerSorted ++ [x] ++ biggerSorted

-- sums of all odd squares that are smaller than 10,000
sumOdds = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- also
sumOdds' = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

{-
	Collatz sequences
	If number is even divide by two
	If odd multiply by three and then add one
	Repeat with the results
	The sequence should always end with one


	How many chains starting with numbers between
	one and a hundred have a length greater than fifteen	
-}

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n : chain (n `div` 2)
	| odd n  = n : chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
	where isLong xs = length xs > 15

{-
	Lambdas
-}

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- generally if you have a function foo a = bar b a
-- it can be rewritten as foo = bar b due to currying

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a->b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- could have also used foldl for the above, but would have required
-- use of ++ instead of cons (:). so foldr is preferred when building
-- up new lists from a list
-- right folds work on infinite lists, left folds do not
-- foldl1 and foldr1 do not require an explicit starting value
-- they use the first item of the list

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- scanl and scanr are like foldl/r only they report all the
-- intermediate accumulator states as a list
-- with scanl the result is the last element
-- with scanr the result is the head

scanlPlus = scanl (+) 0 [3,5,2,1]
scanrPlus = scanr (+) 0 [3,5,2,1]

-- how many elements does it take for the sum
-- of the roots of all natural numbers to exceed 1000?

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- $ makes function application right-associative
-- sum (map sqrt [1..130] can be rewritten
-- sum $ map sqrt [1..130]

-- function composition using (.)

notcomposed = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
composed = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

