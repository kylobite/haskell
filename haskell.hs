-- Real World Haskell & Learn You A Haskell

-- It --
x = 0
succ x --- 1
y = it --- 1

-- Math --
(+) 0 1         --- 1
sqrt 16         --- 4
succ 32         --- 33
pred 64         --- 63
sin (pi / 2)    --- 1.0
truncate pi     --- 3
round pi        --- 3
round 3.5       --- 4
floor 3.9       --- 3
ceiling 3.1     --- 4
odd (round pi)  --- True
even (round pi) --- False

-- Compare --
compare 0 1                 --- LT
compare 1 1                 --- EQ
compare 1 0                 --- GT
compare (sqrt 4) (sqrt 9)   --- LT
compare 0 1 == LT           --- True

-- Head, Tail, Init, Last --
list = ["K","A","I"]
head list --- "K"
tail list --- ["A","I"]
init list --- ["K","A"]
last list --- "I"

-- Tuples --
bday = (102613, "KAI")
fst bday --- 102613
snd bday --- "KAI"

-- Take, Drop --
take 2 list --- "KA"
drop 1 list --- "AI"

-- Infinite List + Lazy --
take 3 [1..] --- [1,2,3]

-- Length --
length [0,2,4,8] --- 4

-- Lines --
lines "K\nA\nI" --- ["K","A","I"]

-- Multiline Drop --
kDrop :: Int -> [a] -> [a]
kDrop x xs = if x <= 0 || null xs
             then xs
             else kDrop (x - 1) (tail xs)

-- One-liner Drop --
kDropX :: Int -> [a] -> [a]
kDropX x xs = if x <= 0 || null xs then xs else kDrop (x - 1) (tail xs)

-- Null --
null "kai" --- False

-- Odd --
isOdd n = mod n 2 == 1
isOdd 2 --- False

-- Or --
False || False --- False

-- Or Function + Short Circuit --
newOr :: Bool -> Bool -> Bool
newOr a b = if a then a else b

newOr True (length [1..] > 0) --- True

-- Type + Data --
type KInt       =    Int
type KString    =    String
type KTuple     =    (KInt, KString)
data KType      =    KType KTuple
                     deriving (Show)

ktype = KType (0, "\0")

-- One-liner Algebraic Data Type --
data Bool = False | True

-- Multiline Algebraic Data Type --
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

-- Distinction of Algebraic Data Types --
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

-- Data Equality Comparisons --
data Roygbiv = Red 
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)
Red == Yellow --- False

-- Unions --
type Vector = (Double, Double)
data Shape  = Circle Vector Double
            | Poly [Vector]

-- Pattern Matching --
myNot True  = False
myNot False = True
--
sumList (x:xs)  = x + sumList xs
sumList []      = 0

-- Deconstruction --
KType (0, "\0")
(KType ki kt)

-- More Pattern Matching --
third (a, b, c) = c
--
complicated (True, a, x:xs, 5) = (a, xs)
--
ktype =  KType (0, "\0")
kInt    (KType ki kt) = ki
kTuple  (KType ki kt) = kt
kInt    (KType 0, "\0") --- 0
kTuple  (KType 0, "\0") --- \0
--- :type kInt
    --- kInt :: KType -> Int

-- Wild Card --
nkInt   (KType ki _)  = ki
nkTuple (KType _  kt) = kt

-- Wild Card + Patter Matching --
badExample (x:xs) = x + badExample xs
--
goodExample (x:xs)  = x + goodExample xs
goodExample _       = 0

-- Record Syntax --
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
                } --- Order does not matter

-- Parameterized Types --
data Maybe a = Just a
             | Nothing
someBool     = Just True
someString   = Just "string"
someNumber   = Just 1.0
someNothing  = Nothing

-- Recursive Types --
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data List a = Cons a (List a)
            | Nil
              deriving (Show)
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil
--- Turn list literal into List type

toList (Cons x xs) = x:toList xs
toList Nil         = []
--- Exercise: List type into list literal

-- Errors --
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

-- Local Variables --
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

-- Shadowing --
foo = let    a = 1
      in let b = 2
         in  a + b
--- 3
bar = let x = 1
      in ((let x = "foo" in x), x)
--- ("foo", 1)
qux a = let a = "foo"
        in  a ++ "eek!"
--- qux "apple"
    --- "fooeek!"

-- Where Clause --
lend2 amount balance =  if amount < reserve * 0.5
                        then Just newBalance
                        else Nothing
    where reserve    =  100
          newBalance =  balance - amount

-- Local Functions --
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

-- Bad Indentation --
--- If you start your code in this column
    --- It is okay to go to this column
--- As long as you do not go back to this column
--
--- Alignment inside functions does not effect
--- the alignment outside of functions

-- Offside Rule and Braces --
kit = let a = 1
          b = 2
          c = 3
      in  a + b + c
kat = let {a = 1; b = 2; c = 3}
      in   a + b + c
--- kit == kat

-- Case --
fromMaybe defval wrapped =
    case wrapped of
        Nothing     -> defval
        Just value  -> value

-- Correctly match variable --
data Fruit = Apple | Orange
apple   = "apple"
orange  = "orange"

whichFruit :: String -> Fruit
whichFruit f = case f of
                "apple"     -> Apple
                "orange"    -> Orange

-- Correctly compare for equality --
nodesAreSame (Node a _ _) (Node b _ _)
    | a == b        = Just a
nodesAreSame _ _    = Nothing

-- Guards --
lend3 amount balance
        | amount <= 0               = Nothing
        | amount > reserve * 0.5    = Nothing
        | otherwise                 = Just newBalance
        where reserve    = 100
              newBalance = balance - amount
--
niceDrop n xs | n <= 0  = xs
niceDrop _ []           = []
niceDrop n (_:xs)       = niceDrop (n - 1) xs

-- More exercises --
kLength :: [a] -> Int
kLength xs | null xs = 0
kLength []           = 0
kLength (_:xs)       = 1 + kLength xs
--- Calculate length
kMean :: Fractional a => [a] -> a
kMean xs | null xs =    0
kMean []           =    0
kMean (x:xs)       =    let a = x + sum xs
                            b = 1 + (length xs)
                            c = fromIntegral b
                        in  a / c
--- Calculate mean

kPalindrome :: [a] -> [a]
kPalindrome xs | null xs =  []
kPalindrome []           =  []
kPalindrome (x:xs)       =  let a = (x:xs)
                                b = reverse a
                            in  a ++ b
--- Make a palindrome

kIsPalindrome :: Eq a => [a] -> Bool
kIsPalindrome xs | null xs =    False
kIsPalindrome []           =    False
kIsPalindrome (x:xs)       =    let a = (x:xs)
                                    b = reverse a
                                in (a == b)
--- Determine if palindrome

factorize :: Int -> [Int]
factorize p = factor p p
    where factor _ 1   =    []
          factor p ps  =    if p `mod` ps == 0
                            then (ps:(factor p (ps - 1)))
                            else factor p (ps - 1)
--- My spin on factorization (lags at 8+ digits)

-- Infix Functions --
a `plus` b = a + b
--
data a `Pair` b = a `Pair` b
                  deriving (Show)
--
foo' = Pair 1 2
bar' = True `Pair` "qux"
--
elem "k" "kylobite"
--- True
4 `elem` [3,1,4,1,5,9]
--- True
--
import Data:List
"ky" `isPrefixOf`   "kylobite"
--- True
"lo" `isInfixOf`    "kylobite"
--- True
"bite" `isSuffixOf` "kylobite"

-- List Manipulations --
-- * (++) :: [a] -> [a] -> [a]
"foo" ++ "bar"
--- "foobar"
[] ++ [1,2,3]
--- [1,2,3]
[True] ++ []
--- [True]

-- * concat :: [[a]] -> [a]
concat [[1,2,3],[4,5,6]]
--- [1,2,3,4,5,6]

-- * reverse :: [a] -> [a]
reverse "foo"
--- "oof"

-- * and :: [Bool] -> Bool
and [True,False,True]
--- False
and []
--- True

-- * or :: [Bool] -> Bool
or [False,False,True]
--- True
or []
--- False

-- * all :: (a -> Bool) -> [a] -> Bool
all odd [1,3,5]
--- True
all odd [1,2,5]
--- False
all []
--- True
-- * any :: (a -> Bool) -> [a] -> Bool
any even [1,3,6]
--- True
any even []
--- False

-- Working With Sublists --
-- * splitAt :: Int -> [a] -> ([a], [a])
splitAt 3 "foobar"
--- ("foo", "bar")

-- * takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile odd [1,3,6,7]
--- [1,3]

-- * dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile even [2,4,5,8]
--- [5,8]

-- * span :: (a -> Bool) -> [a] -> ([a], [a])
span even [2,4,5,8]
--- ([2,4], [5,8])

-- * break :: (a -> Bool) -> [a] -> ([a], [a])
break even [1,3,6,7]
--- ([1,3], [6,7])

-- Searching Lists --
-- * filter :: (a -> Bool) -> [a] -> [a]
filter odd [1,2,3,4]
--- [1,3]

-- Work With Several Lists --
-- * zip :: [a] -> [b] -> [(a, b)]
zip [1,2,3] "kylobite"
--- [(1, "k"), (2, "y"), (3, "l")]

-- * zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith (+) [1,2,3] [4,5,6]
--- [5,7,9]

-- String Handling --
lines "foo\nbar"
--- ["foo", "bar"]

unlines ["foo", "bar"]
--- "foo\nbar\n"

words "the\rquick\tbrown\n\nfox"
--- ["the", "quick", "brown", "fox"]

unwords ["jumps", "over", "the", "lazy", "dog"]
--- "jumps over the lazy dog"

-- Loops and Recursion --
import Data.Char (digitToInt)
asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc []     = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

asInt "42"
--- 42
asInt ""
--- 0
asInt "kylobite"
--- *** Exception: Char.digitToInt: not a digit 'k'

-- Transform Input --
square :: [Double] -> [Double]
square (x:xs) = x*x : square xs
square []     = []

import Data.Char (toUpper)
upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
upperCase []     = []

-- Mapping Over A List --]
square' xs = map squareOne xs
    where squareOne x = x * x

upperCase' = map toUpper xs

myMap :: (a -> b) -> [a] -> [b
myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []

map toUpper "kylobite"
--- "KYLOBITE"
map negate [1,2,3]
--- [-1,-2,-3]

-- Select Pieces Of Input --
oddList :: [Int] -> [Int]
oddList (x:xs) | odd x     = x : oddList xs
               | otherwise = oddList xs
oddList _                  = []

oddList [1,2,3,4,5]
--- [1,3,5]

-- * filter :: (a -> Bool) -> [a] -> [a]
filter odd [1,2,3,4,5]
--- [1,3,5]

-- Computing One Answer Over Collection --
mySum :: [Int] -> Int
mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc

mySum [1,2,3,4]
--- 10

{-| Adler32
    
    This is a checksum that concats two 16-bit checksums
    The first checksum is the sum of input bytes plus 1
    The second is the sum of every other value in the first
    The sums are passed through a modulus of 66521

-}

import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

base = 66521

adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b _      = (b `shiftL` 16) .|. a
--- This is complicated

adler32' xs = helper (1,0) xs
    where helper (a,b) (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                    b' = (a' + b) `mod` base
                                in helper a' b' xs
          helper (a,b) _      = (b `shiftL` 16) .|. a
--- This is not much better, but remember this

-- The Left Fold --
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' step zero (x:xs) = foldl' step (step zero x) xs
foldl' _    zero []     = zero

foldSum :: [Int] -> Int
foldSum xs = foldl step 0 xs
    where step acc x = acc + x

niceSum :: [Int] -> Int
niceSum xs = foldl (+) 0 xs

{-| foldl

    foldl (+) 0 (1:2:3:[])
        == foldl (+) (0 + 1)             (2:3:[])
        == foldl (+) ((0 + 1)+ 2)        (3:[])
        == foldl (+) (((0 + 1) + 2) + 3) []
        ==           (((0 + 1) + 2) + 3)

-}

-- Remeber Adler32?
adler32_foldl xs = let (a,b) = foldl step (1,0) xs
                   in (b `shiftL` 16) .|. a
    where step (a,b) x = let a' = a + (ord x .&. 0xff)
                         in (a' `mod` base, (a' + b) `mod` base)

-- Folding From The Right --
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' step zero (x:xs) = step x (foldr' step zero xs)
foldr' _    zero []     = zero

{-| foldr

    foldr (+) 0 (1:2:3:[])
        == 1 +           foldr (+) 0 (2:3:[])
        == 1 + (2 +      foldr (+) 0 (3:[]))
        == 1 + (2 + (3 + foldr (+) 0 []))
        == 1 + (2 + (3 + 0))

    1 : (2 : (3 : []))
    1 + (2 + (3 + 0 ))

-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []    = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys
-- Don't forget these pipes are like if-else statements

-- A function expressed using foldr is 'primitive recursion'

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = foldr step [] xs
    where step x ys = f x : ys

-- Fasten your seat belts kids
-- We are making Foldl with Foldr

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)
-- * id :: a -> a

-- Foldr transforms its input list

identity :: [a] -> [a]
identity xs = foldr (:) [] xs

identity [1,2,3]
--- [1,2,3]

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
-- What can't foldr do?

append [1,2] [3,4]
--- [1,2,3,4]

-- Laziness and Space Leaks --
-- foldl (+) [0..1000000] is not lazy and can cause space leaks
--import Data.List
foldl' (+) [0..1000000]
--- 500000500000

-- Lambdas!!! --
isInAny_partial needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s

isInAny_lambda needle haystack = any (\s -> needle `isInfixOf` s) haystack
--
safeHead (x:_) = Just x
safeHead _     = Nothing

unsafeHead :: [a] -> a
unsafeHead = \(x:_) -> x

-- unsafeHead []
-- Exception: Non-exhaustive pattern in lambda
-- Be careful with your lambdas!

-- Partial Function Application and Currying --
-- * dropWhile :: (a -> Bool) -> [a] -> [a]
-- * dropWhile isSpace :: [Char] -> [Char]
map (dropWhile isSpace) ["a ","b"," c"]
--- ["a","b","c"]

-- * zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 "foo" "bar" "baz"
--- [('f','b','b'),('o','a','a'),('o','r','z')]

-- * zip3 "foo" :: [b] -> [c] -> [(Char, b, c)]
zip3foo = zip3 "foo"

zip3foo "bar" "baz"
--- [('f','b','b'),('o','a','a'),('o','r','z')]

-- * zip3 "foo" "bar" :: [c] -> [(Char, Char, c)]
zip3foobar = zip3 "foo" "bar"

zip3foobar "baz"
--- [('f','b','b'),('o','a','a'),('o','r','z')]

isInAny_curry needle haystack = any (isInfixOf needle) haystack

niceCurrySum :: [Int] -> Int
niceCurrySum xs = foldl (+) 0 xs

niceeCurrySum :: [Int] -> Int
nicerCurrySum   = foldl (+) 0
-- Quick side note: Use Integer over Int for super big ints

-- Sections --
(1+) 2
--- 3

map (*3) [1,2]
--- [3,6]

map (2^) [3,4]
--- [8,16]

-- * (`elem` ["a".."z"]) :: Char -> Bool
(`elem` ["a".."z"]) "e"
--- True

all (`elem` ["a".."z"]) "Fizzbuzz" -- all lowercase?
--- False

isInAny_section needle haystack = all (needle `isInfixOf`) haystack

-- As-Patterns --
tail ("foobar")
--- "oobar"
tail (tail "foobar")
--- "obar"

--import Data.List
-- * tails :: [a] -> [[a]]
tails "foobar"
--- ["foobar","oobar","obar","bar","ar","r",""]
-- I don't know about you, but this is cool
-- There is also `inits`

tails []
--- [[]]

-- * suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _          = []
-- @ is an "as-pattern"
-- It binds the left to the value that matches the right

tails "foo"
--- ["foo","oo","o",""]
suffixes "foo"
--- ["foo","oo","o"]

-- To show the 'non-as-pattern' version:
suffixesNoAt :: [a] -> [[a]]
suffixesNoAt (x:xs) = (x:xs) : suffixesNoAt xs
suffixesNoAt _      = []

-- Code Reuse Through Composition --

-- Suffixes and tails are similar, let's make use of that
suffixes' :: [a] -> [[a]]
suffixes' xs = init (tails xs)

suffixes' "foo"
--- ["foo","oo","o"]

-- "If A equals B, and B equals C, then A must equal C"
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes'' xs = compose init tails xs
-- We can do better
suffixes''' = compose init tails

--Plot twist, Haskell comes with a compose function! *Dun dun duuunnnn!*
-- * (.) :: (b -> c) -> (a -> b) -> a -> c
suffixes_comp = init . tails

-- Word of caution: Types need to match up to use (.)
-- Remember (.)'s type signature!

import Data.List (isUpper)
capCount :: String -> Int
capCount = length . filter (isUpper . head) . words
-- That's a lot of functions...

capCount "How Many Capital Letters Are Here?"
--- 6

-- Avoiding Space Leaks With Seq --
fold' _    zero []     = zero
fold' step zero (x:xs) =
    let new = step zero x
    in  new `seq` foldl' step new xs

-- * seq :: a -> t -> t
-- Seq evals the 1st arg, then return the 2nd arg
-- It is used to force arguments to be evaluated

{-|
    
    foldl' (+) 1 (2:[])
        let new = 1 + 2
        in  new `seq` foldl' (+) new []
        foldl' (+) 3 []
        3

-}

-- Learning To Use Seq --
strictPair :: (a,b) -> (a,b)
strictPair (a,b) = a `seq` b `seq` (a,b)

strictList :: [a] -> [a]
strictList (x:xs) = x `seq` x : strictList xs
strictList []     = []

-- Representing JSON Data in Haskell --
data JValue = JString String
            | JNumber Double
            | JBool   Bool
            | JObject [(String,JValue)]
            | JArray  [JValue]
            | JNull
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing
-- truncate :: Double -> Int

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String,JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull :: JValue -> Bool
isNull v            = (v == JNull)

-- The Anatomy of a Haskell Module --
-- See json/*

-- Compiling Haskell Code --
-- ghc -c [source code]
--- Generates only object code (.hi, .o)
---   .hi - interface file
---   .o  - object file
--- ghc -o [program name] [source code]
-- undefined :: a ; Used as a TODO value

-- The Need for Typeclasses --
--- Situation: Equity test `==` is removed. What do?
data Color = Red | Blue | Green

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Blue  Blue  = True
colorEq Green Green = True
colorEq _     _     = False
-- Custom type checking

stringEq :: [Char] -> [Char] -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys -- Yes, we cheated
stringEq _  _  = False
-- String type checking

-- Problem: We need to do this for every type, and have to cheat as well

-- What are Typeclasses? --
class BasicEq a where
    isEqual :: a -> a -> Bool
-- Class is not an OOP class

instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False
-- We can use instances of typeclasses for certain types (e.g. Bool)
-- This particular instance only works for Bool, nothing else

-- We should probably add a not-equal function

class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool

-- Hmm, not good enough

class BasicEq3 a where
    isEqual3    :: a -> a -> Bool
    isEqual3 x y    = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)

{-| The actual Eq typeclass

    class Eq a where
        (==),(/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)

-}

-- Declaring Typeclass Instances --
instance BasicEq3 Color where
    isEqual3 Red   Red   = True
    isEqual3 Green Green = True
    isEqual3 Blue  Blue  = True
    isEqual3 _     _     = False
-- `isEqual3` is not defined, so the default is used
-- This as well solves the problem in `The Need for Typeclasses`

-- The Show Typeclass --
-- * show :: (Show a) => a -> String
-- `show` converts data to a String

show 1       --- "1"
show [1,2,3] --- "[1,2,3]"
show (1, 2)  --- "(1,2)"

putStrLn (show 1)       --- 1
putStrLn (show [1,2,3]) --- [1,2,3]

show "String"            --- "\"String\""
putStrLn (show "String") --- "String"
-- It is less confusing to avoid `show` on Strings

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"
-- Define custom types their `show` version

















































