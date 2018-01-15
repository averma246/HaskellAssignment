import StackModule

-----------------------------------------------------------------------------------
                             -- HOMEWORK #2 --
                              -- ANA VERMA -- 
                           -- ALLAN J PHILLIPS--
-----------------------------------------------------------------------------------                              
{-============================================================
QUESTION #1
==============================================================-}

{- (A) -}
-----------------------------------------------------------------------------------
--returns a list that starts with 0 and continues by adding two
--resulting in a list of all evens 

evens :: [Int]
evens = iterate (+2) 0
-----------------------------------------------------------------------------------


{- (B) -}
-----------------------------------------------------------------------------------
--filters out all the negative numbers from the given list and then maps the double 
--function onto each element of this filtered list 

{-MAIN FUNCTION-}
doublePos :: [Int] -> [Int]
doublePos x = map double (filter (>0) x)

{-HELPERS-}
double :: Int -> Int
double x = 2*x
-----------------------------------------------------------------------------------


{- (C) -}
-----------------------------------------------------------------------------------
--returns a list whose first value is the initial guess, and the rest is a list of 
--the updated guesses 

{-MAIN FUNCTION-}
-- n = number you want the square root of
-- x = initial guess 

guessRoot ::  Float -> Float -> [Float]
guessRoot n x = iterate (guess n) x

{-HELPERS-}
--updates the guess 
guess :: Fractional a => a -> a -> a
guess n x = (x + n/x)/2
-----------------------------------------------------------------------------------


{- (D) -}
-----------------------------------------------------------------------------------
--first makes a list of ordered pairs by zipping up the given list and its cdr
--then filters out all the ordered pairs that have a difference of greater than n 
--then takes the first element of this list of ordered pairs, and returns the second 
--of the two numbers

{-MAIN FUNCTION-}
-- x = list of numbers, n = number that dictates precision

precision :: [Float] -> Float -> Float
precision x n = secondInPair ( firstElem (filter (checkDiff n) (makePairs x)))

{-HELPERS-}

--zips the given list with its cdr 

makePairs :: [Float] -> [(Float,Float)]
makePairs (x:xs) = zip (x:xs) xs

--checks the difference between the given pair of numbers and returns true if the 
--difference is within the range given

checkDiff :: Float -> (Float,Float) -> Bool
checkDiff n (x,y) = ( (abs (x-y)) <= n)

--returns the first element of a list (in this case, the first ordered pair that
--satisfies the given conditions)

firstElem :: [a] -> a
firstElem (x:xs) = x

--returns the second of two numbers in a pair 
secondInPair :: (Float, Float) -> Float
secondInPair (x,y) = y
-----------------------------------------------------------------------------------


{- (E) -}
-----------------------------------------------------------------------------------
--Uses the two functions written above -- guessRoot will return all the guesses of the 
--square root and precision will return the guess that is at least as precise as requested

{-MAIN FUNCTION-}

-- x = number you want the square root of
-- t = number dictating precision

root :: Float -> Float -> Float
root x t = precision (guessRoot x (x/2)) t
-----------------------------------------------------------------------------------


{- (F) -}
-----------------------------------------------------------------------------------
--takes in a list of functions and a list of values
--zips up these two lists to make a list of ordered pairs 
--each ordered pair then contains a function and a value

{-MAIN FUNCTION-}
combine :: [(b -> a)] -> [b] -> [a]
combine f b = map doFunction (zip f b) 

{-HELPERS-}

--applies the function in the ordered pair to the 
--second element of the pair, which is a value 

doFunction :: ((b -> a),b) -> a
doFunction (f,x) = f x
-----------------------------------------------------------------------------------


{- (G) -}
-----------------------------------------------------------------------------------
{-MAIN FUNCTION-}

-- f = list of functions
-- b = value

mapValue :: [(b -> a)] -> b -> [a]
mapValue f b = map (applyFunction b) f

{-HELPERS-}

--applies a given function onto the given value

applyFunction :: b -> (b -> a) -> a 
applyFunction b f = f b 
-----------------------------------------------------------------------------------


{- (H) -}
-----------------------------------------------------------------------------------
remove :: Eq a => a -> [a] -> [a]
remove c x = filter (/= c) x
-----------------------------------------------------------------------------------


{- (I) -}
-----------------------------------------------------------------------------------
--removes all elements from the first list passed in from the second list

removeList :: Eq a => [a] -> [a] -> [a]
removeList x y = foldr remove y x
-----------------------------------------------------------------------------------


{- (J) -}
-----------------------------------------------------------------------------------
{-MAIN FUNCTION-}

prefixes :: [a] -> [[a]]
prefixes x = foldr addToPrefixes [[]] x

{-HELPERS-}
addToPrefixes :: a -> [[a]] -> [[a]]
addToPrefixes x [] = [[]]
addToPrefixes x (y:ys) = y :(map ((++) [x]) (y:ys)) 
-----------------------------------------------------------------------------------


{- (K) -} 
-----------------------------------------------------------------------------------
{-MAIN FUNCTION-}
powerset :: [a] -> [[a]]
powerset x = foldr addSet [[]] x

{-HELPERS-}

--concatenates value onto each elment of the list and then concatenates this
--new list and the old that was passed in 

addSet :: a -> [[a]] -> [[a]]
addSet x y =  (++) y (map ((++) [x]) y)
-----------------------------------------------------------------------------------


{-===============================================================
QUESTION #2
===============================================================-}
-----------------------------------------------------------------------------------
{-MAIN FUNCTION-}
insertionSort:: Ord a => [a] -> [a]
insertionSort x = foldr sortHelper [] x


{-HELPERS-}

--breaks down the sorted list 

sortHelper :: Ord a => a -> [a] -> [a]
sortHelper x y = foldr sortHelper2 [x] y


--checks whether a value is greater than the first value of a list
--if it is, then it becomes the second element of that list
--it it is not, then it becomes the first element of that list (which 
--is the sorted list)

sortHelper2 :: Ord a => a -> [a] -> [a]
sortHelper2 x (y:ys)
 | (x >y) = y:x:ys
 | otherwise = x:y:ys
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------


{-===============================================================
QUESTION #3
===============================================================-}
-----------------------------------------------------------------------------------
--takes in a "book" and breaks it down into the separate pages
--the "z" of foldl in this case is [("page#s", [1])] -- the first 
--element of this list is an ordered pair that will simply serve 
--as a counter for the current page number -- it is removed from 
--the final result 
--the words in the ordered pairs appear in the order they are first 
--as the "book" is "read"

{-MAIN FUNCTION-}
invIndex :: [[String]] -> [(String,[Int])]
invIndex x = tail(foldl scanPage [("page#",[1])] x)

{-HELPERS-}

-----------------------------------------------------------------------------------
--scanPage takes a page and breaks it down into the words on that page 
--the first element of the accumulating list will contain the page
--number and as scanPage is called over and over again, the 
--page number counter increases by one

-- z = current page number
-- ys = cdr of the list of ordered pairs (so the list that will actually be returned)
-- x = page 

scanPage :: [(String,[Int])] -> [String]  -> [(String,[Int])]
scanPage ((y,(z:zs)):ys) x = (y,((z+1):zs)) : foldl (checkString z) ys x 

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

--checks whether a string is already in the accumulating list of ordered pairs 
--if the string is already in the list, then the accumulating list is broken down 
--until the string is found and then the list of pages for that string is updated

--if the string is not in that ordered pair list, then a new ordered pair is created with the current 
--page as the first value in the list of pages for that string

-- n = page number 
-- list = list of ordered pairs
-- s = the current word that is being checked

checkString :: Int -> [(String,[Int])] -> String -> [(String,[Int])]
checkString n list s
  | (contains list s) = foldl (insertIndex n s) [] list
  | otherwise = list ++ [(s,[n])] 
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

-- checks whether a given string is already in the list of ordered pairs

contains :: [(String,[Int])] -> String -> Bool
contains [] s = False
contains ((x,y):xs) s 
  |(x == s) = True
  |otherwise = contains xs s
  
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

--goes through the accumulating list to find the ordered pair containing the string and updates the list of ints  

insertIndex :: Int -> String -> [(String,[Int])] -> (String,[Int])-> [(String,[Int])]
insertIndex n s z (y,x) 
  |(s == y) && not(elem n x)= z ++ [(y,x ++ [n])]
  |otherwise = z ++ [(y,x)]

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------


{-===============================================================
QUESTION #4
===============================================================-}

{-MAIN FUNCTION-}
-----------------------------------------------------------------------------------
primes :: [Int]
primes = foldr filterNonPrimes (zoomUp 2)(zoomUp 2)
-----------------------------------------------------------------------------------

{-HELPERS-}
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

--ZoomUp makes an infinite list of consecutive numbers that start from the given into

zoomUp :: Int -> [Int]
zoomUp x = x : zoomUp (x+1)
-----------------------------------------------------------------------------------

--notFactor checks whether a number is a multiple of the other 

notFactor :: Int -> Int -> Bool
notFactor x y = ((mod y x) /= 0)
-----------------------------------------------------------------------------------

--filterNonPrimes essentially takes a list and removes all multiples 
--of a given number 

filterNonPrimes :: Int -> [Int] -> [Int]
filterNonPrimes x y = x :(filter (notFactor x) y) 
-----------------------------------------------------------------------------------



-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------



{- Question 5 -}
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

-- These testers just check out all the functions that are imported 
-- from the StackModule

tester0 = initStack 7
tester1 = push 21 (tester0)
tester2 = push 13 (tester1)
tester3 = push 15 (tester2)
tester4 = peek tester3
tester5 = pop tester3
tester6 = isEmpty tester3
tester7 = isEmpty (pop tester0)
