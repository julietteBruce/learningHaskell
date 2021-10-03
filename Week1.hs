

-- Creates a list with digits of an integer read right to left
-- If the number is not positive, returns then empty list
--
-- Note: We do toDigitsRev first since prepending might(?)
-- be faster than apending for longer lists
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
     | n <= 0 = []
     | otherwise  = (n `mod` 10):toDigitsRev (n`div`10)

-- Tests for toDigitsRev
-- toDigitsRev 1234 == [4,3,2,1]

-- Creates a list with digits of an integer read left to right
-- If the number is not positive, returns then empty list
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Tests for toDigits
-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Takes an integer and doubles it
doubleNumber :: Integer -> Integer
doubleNumber n = n + n

-- Takes a list of integers and doubles every other starting from the left,
-- i.e. doubles every element in an odd position, leaving the other unchanged.
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = [] -- apparently the empty list is a list of integers
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:(y:zs)) = x:(doubleNumber y):(doubleEveryOtherRev zs)

-- Tests for doubleEveryOtherRev
-- doubleEveryOtherRev [8,7,6,5] == [8,14,6,10]

-- Takes a list of integers and doubles every other starting from the right,
-- leaving the other unchanged.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherRev (reverse x))

-- Tests for doubleEveryOther
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Takes a list of positive natural numbers and returns a list consisting of
-- the digits of the original numbers as they appear from left to right.
--
-- Warning: If any of the numbers are <=0 then those numbers are dropped from
-- the returned list. 
toDigitsList :: [Integer] -> [Integer]
toDigitsList [] = []
toDigitsList (x:ys) = toDigits(x)++(toDigitsList ys)

-- Tests for toDigitsList
--toDigitsList [16,7,12,5] == [1,6,7,1,2,5]

-- Sums all of the digits in a list of positive natural numbers.
--
-- Warning: If any of the numbers are <=0 then those numbers are dropped from
-- the returned sum. 
sumDigits :: [Integer] -> Integer
sumDigits n = sum (toDigitsList n)

-- Tests for sumDigits
-- sumDigits [16,7,12,5] == 22

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Checks whether a credit card number is valid.
--
-- Warning: If the number is <=0 then returns false always. Persumably
-- crediate card numbers can't be 0 or negative. 
validate :: Integer -> Bool
validate n
     | n <= 0 = False
     | ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0 = True
     | otherwise = False


-- Tests for validate
-- validate 4012888888881881 == True
-- validate 4012888888881882 == Flase

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

type Peg = String
type Move = (Peg, Peg) -- (a,b) means move disc peg a to peg b


-- Returns a list of moves needed to be performed in order to move
-- n disc in The Towers of Hano (with pegs a b c) from peg a to
-- peg b. The moves are returned in order.
--
-- Idea: A recursive strategy to move n disc from a to b is to:
-- 1) move n-1 discs from a to b using c as temp storage.
-- 2) move the disc on a to b
-- 3) move n-1 discs from c to b using a as temp storage.
-- Notice hanoi is a function that moves n discs from a to b
-- using c as temp storage. So the above steps are just
-- hanoi with the order of a, b, and c changed as follows:
-- 1) (hanoi (n-1) a c b)
-- 2) (hanoi 1 a b c)
-- 3) (hanoi (n-1) c b a)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi 1 a b c = [(a,b)]
hanoi n a b c = (hanoi (n-1) a c b)++(hanoi 1 a b c)++(hanoi (n-1) c b a)


--Test for hanoi
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- hanoi 3 "a" "b" "c" == [("a","b"), ("a","c"), ("b","c"), ("a","b"), ("c","a"), ("c","b"), ("a","b")]
