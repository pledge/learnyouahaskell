{-
	:t 'a' - determines the type of expression
	:: is read as "has type of"
-}

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]


{-
	Common Types
		Int
		Integer - unbounded integer, less efficient
		Float - single precision
		Double
		Bool
		Char

		
	Typeclasses
		Typeclasses is an interface that defines some behaviour
		=> is a class constraint

		Common Types
			Eq - equality testing
			Ord - ordering
			Show - can be presented as a string
			Read - takes a string and returns a member of Read
			Enum - sequentially ordered, can be enumerated
			Bounded - upper and lower bounds
			Num
			Integral
			Floating - see fromIntegral

-}

comparisonResult = 'a' `compare` 'z'
threeAsString = show 3
threeRead = read "3" :: Int -- explicit type annotation removes ambiguity 
readPair = read "(3, 'a')" :: (Int, Char)

minInt = minBound :: Int -- can be thought of as a polymorphic constant
maxChar = maxBound :: Char
maxTuple = maxBound :: (Bool, Int, Char)
