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


