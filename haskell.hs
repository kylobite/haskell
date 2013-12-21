-- *
--- learn.hs
-- *

-- It
x = 0
succ x --> 1
y = it --> 1

-- Math
(+) 0 1         --> 1
sqrt 16         --> 4
succ 32         --> 33
pred 64         --> 63
sin (pi / 2)    --> 1.0
truncate pi     --> 3
round pi        --> 3
round 3.5       --> 4
floor 3.9       --> 3
ceiling 3.1     --> 4
odd (round pi)  --> True
even (round pi) --> False

-- Compare
compare 0 1                 --> LT
compare 1 1                 --> EQ
compare 1 0                 --> GT
compare (sqrt 4) (sqrt 9)   --> LT
compare 0 1 == LT           --> True

-- Head, Tail, Init, Last
list = ["K","A","I"]
head list --> "K"
tail list --> ["A","I"]
init list --> ["K","A"]
last list --> "I"

-- Tuples
bday = (102613, "KAI")
fst bday --> 102613
snd bday --> "KAI"

-- Take, Drop
take 2 list --> "KA"
drop 1 list --> "AI"

-- Infinite List + Lazy
take 3 [1..] --> [1,2,3]

-- Length
length [0,2,4,8] --> 4

-- Lines
lines "K\nA\nI" --> ["K","A","I"]

-- Multiline Drop
kDrop :: Int -> [a] -> [a]
kDrop x xs = if x <= 0 || null xs
             then xs
             else kDrop (x - 1) (tail xs)

-- One-liner Drop
kDropX :: Int -> [a] -> [a]
kDropX x xs = if x <= 0 || null xs then xs else kDrop (x - 1) (tail xs)

-- Null
null "kai" --> False

-- Odd
isOdd n = mod n 2 == 1
isOdd 2 --> False

-- Or
False || False --> False

-- Or Function + Short Circuit
newOr :: Bool -> Bool -> Bool
newOr a b = if a then a else b

newOr True (length [1..] > 0) --> True

-- Type + Data
type KInt       =    Int
type KString    =    String
type KTuple     =    (KInt, KString)
data KType      =    KType KTuple
                     deriving (Show)

ktype = KType (0, "\0")

-- One-liner Algebraic Data Type
data Bool = False | True

-- Multiline Algebraic Data Type
type CustomerID  = String
type CardHolder  = String
type CardNumber  = String
type Address     = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
CreditCard "123456" "Some One" ["City", "Country"]
CashOnDelivery
Invoice "customer0000"

-- Distinction of Algebraic Data Types
a = ("metal", "grey")
b = ("water", "blue")
data Solid  = Solid String String
data Liquid = Liquid String String
c = Solid   "metal" "grey"
d = Liquid  "water" "blue"
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)
data Polar2D     = Polar2D Double Double
                   deriving (Eq, Show)
-- Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2 evals to error
-- Eq: allows for equality comparisons

-- Data Equality Comparisons
data Roygbiv = Red 
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)
Red == Yellow --> False

-- Unions
type Vector = (Double, Double)
data Shape  = Circle Vector Double
            | Poly [Vector]

-- Pattern Matching
myNot True  = False
myNot False = True
--
sumList (x:xs)  = x + sumList xs
sumList []      = 0

-- Deconstruction
KType (0, "\0")
(KType ki kt)

-- More Pattern Matching
third (a, b, c) = c
--
complicated (True, a, x:xs, 5) = (a, xs)
--
ktype =  KType (0, "\0")
kInt    (KType ki kt) = ki
kTuple  (KType ki kt) = kt
kInt    (KType 0, "\0") --> 0
kTuple  (KType 0, "\0") --> \0
--> :type kInt
    --> kInt :: KType -> Int

-- Wild Card
nkInt   (KType ki _)    = ki
nkTuple (KType _    kt) = kt

-- Wild Card + Patter Matching
badExample (x:xs) = x + badExample xs
--
goodExample (x:xs)  = x + goodExample xs
goodExample _       = 0

-- Record Syntax
data Thing = Thing {
      thingID       :: Int,
      thingName     :: String,
      thingAddress  :: [String]
      } deriving (Show)
-- ^ simple | this sucks v
data Thing = Thing Int String [String]
             deriving (Show)
thingID :: Thing -> Int
thingID (Thing id _ _) = id
thingName :: Thing -> String
thingName (Thing _ name _) = name
thingAddress :: Thing -> [String]
thingAddress (Thing _ _ address) = address

thing1 = Thing 0 "Thing" ["Thing","1"]
thing2 = Thing {
                thingID     = 0,
                thingAddress = ["Thing","2"],
                thingName    = "Thing"
                } --> Order does not matter

-- Parameterized Types
data Maybe a = Just a
             | Nothing
someBool     = Just True
someString   = Just "string"
someNumber   = Just 1.0
someNothing  = Nothing

-- Recursive Types
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data List a = Cons a (List a)
            | Nil
              deriving (Show)
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil
--> Turn list literal into List type

toList (Cons x xs) = x:toList xs
toList Nil         = []
--> Exercise: List type into list literal

-- Errors
mySecond :: [a] -> a

mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)
--
safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))
--
tidySecond :: [a] -> Maybe a

tidySecond (_:xs:_) = Just xs
tidySecond _        = Nothing

-- Local Variables
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

-- Shadowing
foo = let a = 1
        in let b = 2
            in a + b
--> 3
bar = let x = 1
        in ((let x = "foo" in x), x)
--> ("foo", 1)
qux a = let a = "foo"
            in a ++ "eek!"
--> qux "apple"
    --> "fooeek!"

-- Where Clause
lend2 amount balance =  if amount < reserve * 0.5
                        then Just newBalance
                        else Nothing
    where reserve    =  100
          newBalance =  balance - amount

-- Local Functions
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

-- Bad Indentation
--> If you start your code in this column
    --> It is okay to go to this column
--> As long as you do not go back to this column
--> Alignment inside functions does not effect
--- the alignment outside of functions

-- Offside Rule and Braces
kit = let a = 1
          b = 2
          c = 3
      in  a + b + c
kat = let {a = 1; b = 2; c = 3}
      in   a + b + c
--> kit == kat

-- Case
fromMaybe defval wrapped =
    case wrapped of
        Nothing     -> defval
        Just value  -> value

-- Correctly match variable
data Fruit = Apple | Orange
apple   = "apple"
orange  = "orange"

whichFruit :: String -> Fruit
whichFruit f = case f of
                "apple"     -> Apple
                "orange"    -> Orange

-- Correctly compare for equality
nodesAreSame (Node a _ _) (Node b _ _)
    | a == b        = Just a
nodesAreSame _ _    = Nothing

-- Guards
lend3 amount balance
        | amount <= 0               = Nothing
        | amount > reserve * 0.5    = Nothing
        | otherwise                 = Just newBalance
        where reserve    = 100
              newBalance = balance - amount
--
niceDrop n xs | x <= 0  = xs
niceDrop _ []           = []
niceDrop n (_:xs)       = niceDrop (n - 1) xs

-- Moar exercises
kLength :: [a] -> Int
kLength xs | null xs = 0
kLength []           = 0
kLength (_:xs)       = 1 + kLength xs

-- DO OTHER EXERCISES!!!























