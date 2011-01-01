doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x * 2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

lostNumbers = [4,8,15,16,23,42]

combined = [1,2,3,4] ++ [9,10,11,12]

smallCat = 'A' : " SMALL CAT"

listSugar = [1,2,3] == 1:2:3:[]

charInMartin = "Martin" !! 2 -- r, indices start at zero

headItem = head [5,4,3,2,1]

tailItems = tail [5,4,3,2,1]

lastItem = last [5,4,3,2,1]

initItems = init [5,4,3,2,1] -- [5,4,3,2]

listLength = length [5,4,3,2,1]

nullList = null [1,2,3] -- False. Empty list check

reverseList = reverse [1,2,3,4,5]

takenList = take 3 [5,4,3,2,1] -- [5,4,3]

dropList = drop 3 [8,4,2,1,5,6] -- [1,5,6]

minItem = minimum [8,4,2,1,5,6]
maxItem = maximum [1,9,2,3,4]

-- also sum, product

isElement = 4 `elem` [3,4,5,6] -- truth of whether item in list (contains)

range = [1..20]
evens = [2,4..20]
decRange = [20..1]
letters = ['a'..'z']

lazyRange = take 24 [13,26..]

cycleList = take 10 (cycle [1,2,3])
repeatList = take 10 (repeat 5)
replicateList = replicate 3 10

firstTenEven = [x*2 | x <- [1..10]]
firstTenEven' = [x*2 | x <- [1..10], x*2 >= 12]
boomBangs xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]
boomBangs' xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]

products = [x*y | x<-[2,5,10], y<-[8,10,11]]
products' = [x*y | x<-[2,5,10], y<-[8,10,11], x*y > 50]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [c |  c <- st, c `elem` ['A'..'Z']]

tupleList = [(1,2), (3,4), (5,6)]
firstItemInPair = fst (8,11) -- fst and snd only work on pairs, not triples etc
secondItemInPair = snd (8,11)

zippedList = zip [1,2,3,4,5] [5,5,5,5,5]
zippedList' = zip [1,2,3,4,5] (replicate 5 5)
