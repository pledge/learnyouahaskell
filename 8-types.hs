data Bool = False | True
-- False and True here are "value constructors"
-- Both type name and value constuctors have to be capital cased

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 -x1) * (abs $ y2 - y1)

-- move a shape on the x and y axis and return a new shape 
-- with same dimensions.
nudge :: Shape -> Float -> Float-> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a)(y1+b)) (Point (x2+a)(y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)


-- Record types

data Person = Person { firstName :: String
	, lastname :: String
	, age :: Int
	, height :: Float
	, phoneNumber :: String
	, flavor :: String
} deriving (Show)

-- automatically created the accessor functions 
-- firstName, lastName, age, height
-- phoneNumber and flavour

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

mondeo = Car {company = "Ford", model = "Mondeo", year = 2010 }
