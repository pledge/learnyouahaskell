maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs

maximumUsingMax :: (Ord a) => [a] -> a
maximumUsingMax [] = error "maximum of empty list"
maximumUsingMax [x] = x
maximumUsingMax (x:xs) = max x (maximumUsingMax xs)

-- Num is not a subclass of Ord, so specify both
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0   = []
	| otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
	| otherwise = a `elem'` xs

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort less ++ [x] ++ quicksort greater
    where 
        less    = [i | i <- xs, i <= x]
        greater = [i | i <-xs, i > x]

quicksort' :: (Ord a) => [a] -> [a]  
quicksort' [] = []  
quicksort' (x:xs) =   
    let smallerSorted = quicksort' [a | a <- xs, a <= x]  
        biggerSorted = quicksort' [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

