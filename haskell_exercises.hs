module haskell_exercises where
import Data.Char
import Data.List
import Test.QuickCheck

--hi
-- 1. Define two functions circ and area that calculate the circumference and the area (respectively) of a circle, if the radius of the circle is r. Use the type Double for arguments and results.
circ :: Double -> Double 
circ r = 2 * pi * r
area :: Double -> Double
area r = pi * r^2


-- 2. Write two functions smallLetter and bigLetter that test whether a character is a small or a big letter (respectively).
smallLetter :: Char -> Bool
smallLetter a   | ord a > 96 && ord a < 123 = True
                | otherwise = False
bigLetter :: Char -> Bool
bigLetter a     | ord a > 64 && ord a < 91 = True
                | otherwise = False


-- 3. Define recursively a function interest that calculates how much money you have after n years, if you start with an amount a and receive p percent of interest per year.
interest :: Double -> Double -> Double -> Double
interest 1 a p = a * (1 + (p / 100))
interest n a p = interest (n - 1) a p * (1 + (p / 100))


-- 4. Write recursive definitions for the following functions on polymorphic lists:
-- a) mylength for the length of a list,
lngth [] = 0
lngth (x:xs) = 1 + lngth xs

-- b) mysum that adds the elements in a list of numbers,
mysum [] = 0
mysum (x:xs) = x + mysum xs

-- c) myreverse that reverses the order of the elements in a list,
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x] 
prop_myreverse xs = myreverse xs == reverse xs

-- d) mytake that gives the first n elements of a list (in case n is greater than the length of a list, the whole list should be delivered),
mytake :: Int -> [a] -> [a]
mytake 0 xs = []
mytake n [] = []
mytake n (x:xs) = x : mytake (n-1) xs
prop_mytake n xs = n >= 0 ==> mytake n xs == take n xs

-- e) myelem that determines whether a given element is in a list,
myelem :: Eq a => a -> [a] -> Bool
myelem n [] = False
myelem n (x:xs) = x == n || myelem n xs
prop_myelem x ys = myelem x ys == elem x ys

-- f) myconcat that glues together a list of lists into one long list,
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (xs:xss) = xs ++ myconcat xss
prop_myconcat xs = myconcat xs == concat xs

-- g) mymaximum that yields the maximum of a list of numbers,
mymax :: Ord a => [a] -> a
mymax [] = error "empty list"
mymax [x] = x
mymax (x:y:xs) | x <= y = mymax (y:xs)
               | otherwise = mymax (x:xs)

-- h) myzip that transforms two lists into a list of pairs (the shortest of the two lists determines the length of the resulting list).
myzip :: [a] -> [b] -> [(a,b)]
myzip as [] = []
myzip [] bs = []
myzip (a:as) (b:bs) = (a,b) : myzip as bs
prop_myzip xs ys = myzip xs ys == zip xs ys


-- 5. Write a function allEqual that determines whether all elements in a list are equal.
allEqual :: [Int] -> Bool
allEqual a | sum a == length a * head a = True
           | otherwise = False


-- 6. Write a function that checks whether a list is a palindrome.
is_palindrome :: Eq a => [a] -> Bool
is_palindrome a | a == reverse a = True
                | otherwise = False


-- 7. Write a function incr that checks whether a list is increasing or not.
incr :: (Ord a) => [a] -> Bool
incr [] = True
incr [a] = True
incr (a:b:xs) = a <= b && incr (b:xs)


-- 8. Define a function dividers that calculates the list of all dividers of a given number n.
dividers n = [a | a <- [1..n], mod n a == 0]


-- 9. Write a function isPrime that tests whether a given number is prime or not.
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime a = dividers a == [1, a]


-- 10. In how many ways can 100 be expressed as the sum of two prime numbers?
primeList = [(x,y) | x <- [1..100], y <- [1..100], isPrime x == True, isPrime y == True, x + y == 100]


-- 11. Write a linear search function that yields the list of all indexes of a required element x in a given list xs.
linearsearch :: Eq a => a -> [a] -> [Int]
linearsearch x xs = [i | (n,i) <- myzip xs [0..length xs - 1], x==n]


-- 12. The algorithm insertion sort starts from an empty list and inserts the elements from the given list one after another on the correct position. First write a function ins that inserts an element on the correct position in an (already sorted) list. Use this function to write the function isort that implements the insertion sort algorithm.
insertx :: Ord a => a -> [a] -> [a]
insertx x [] = [x]
insertx x (y:ys) | x < y = x:y:ys
                 | otherwise = y:(insertx x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insertx x (isort xs)
prop_isort xs = isort xs == sort xs