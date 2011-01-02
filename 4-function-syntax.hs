-- pattern matching

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- ordering is important
-- if the x form was first no others would match
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- without pattern matching
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' a b = (fst a + fst b, snd a + snd b)

-- with pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

{-
	Pattern matching on lists
	[1,2,3] is syntactic sugar for 1:2:3:[]
	x:xs in a pattern match will make x the head and xs the rest
	It will only match against lists of length one or more
	x:y:z:zs will match the first three items, but will only match 
	on lists with three or more items
-}

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs 

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- "as patterns", breaks up using a pattern whilst still keeping a reference to the whole thing
-- uses a name and @ in front of a pattern

capital :: String -> String
capital "" = "Empty string, whoops"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

{-
	Gaurds
	A way of testing whether some property of a value (or several of them)
	are true of false
-}

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b     = a
	| otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b     = GT
	| a == b    = EQ
	| otherwise = LT

-- where
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where (f:_) = firstname
	      (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

-- let
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

-- case expressions
head'' :: [a] -> a
head'' [] = error "No head for empty lists!"
head'' (x:_) = x

-- above is syntactic sugar for below
head''' :: [a] -> a
head''' xs = case xs of [] -> error "No head for empty lists!"
                        (x:_) -> x
