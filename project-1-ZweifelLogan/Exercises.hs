module Exercises(module Exercises) where
import Data.List

test :: Int -> Int
test x = x + 1

--TODO: Implement some error handling


--------------------
-- 2) Reading and Writing comparator networks
--this takes single integer tuple and prints/shows it in the right way
-- will need to run on entire list and write each element on a newline to a txtfile

--val = [(1,2),(3,4),(1,3),(2,4),(2,3)] -- testing val with data from sort1.txt

-- takes list of tuple ints, converts them into the desired string format and puts them back in a list
t2 :: [(Int, Int)] -> String
t2 [] = ""
t2 ((x,y):xs) = show x ++ " -- " ++ show y ++ "\n" ++ t2 xs

---- END #2

----------------------
--3) Need to implement a comparator network on an input sequence, refer to links on assignment page on how to do this

--testing values are val from #2 (data from sort1.txt) and below as the sequence

--sequen = [5,1,3,0]

t3 :: [Int] -> [(Int, Int)] -> [Int]
t3 s [] = s
t3 s ((x,y):xs) =
    if comp3 s (x-1, y-1) == Just True then
        t3 (replace4 (x-1) (y-1) s) xs
    else
        t3 s xs

comp3 :: [Int] -> (Int, Int) -> Maybe Bool
comp3 s (x, y) | (x < length s) && (y < length s) = Just ((s !! x) > (s !! y))
               | otherwise = Nothing

replace4 :: Int -> Int -> [Int] -> [Int]
replace4 i j xs =
    let elemI = xs !! i
        elemJ = xs !! j
        left = take i xs
        middle = take (j - i - 1) (drop (i + 1) xs)
        right = drop (j + 1) xs
    in  left ++ [elemJ] ++ middle ++ [elemI] ++ right

--val for testing the results on the example Input and output

--val3 = t3 sequen val -- this is the desired output in normal list format

--- END #3

-------------------------
--4) Putting comparator networks into parallel form
-- (x,y) and (a,b) may be done in parallel when the sets of the two are disjoint from one another, can be executed simultaneously

-- Test input is data from sort1.txt which is val in this program
-- Test output is data in parallel1.txt


--t4 val outputs the right list, but now must format it correctly to be printed
t4 :: [(Int,Int)] -> [[(Int,Int)]]
t4 [] = []
t4 ((x,y):xs) =
    let z = (x,y) : search4x ((x,y):xs) in
        z : t4 (removeitems z xs)


-- val = [(1,2),(3,4),(1,3),(2,4),(2,3)]


search4x :: [(Int,Int)] -> [(Int,Int)]
search4x [] = []
search4x [(_,_)] = []
search4x ((x,y):(a,b):xs) =
    if (x /= a) && (x /= b) && (y /= a) && (y /= b) then
        (a,b) : search4x ((x,y):xs)
    else
        search4x ((x,y):xs)

---             in parallel     full list
removeitems :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
removeitems [] zs = zs
removeitems ((x,y):xs) zs = removeitems xs (delete (x,y) zs)

--val4 = t4 val -- testing value for the format of the list 

-- TODO: come back and implement this sort function for if data is out of order
sort4 :: [(Int, Int)] -> [(Int,Int)]
sort4 [] = []
sort4 ((x,y):xs) = undefined

format4 :: [[(Int,Int)]] -> String
format4 = concatMap format4b -- take all sublists after becoming strings in fucntion below into one big string to be written to file
-- above is the same as below
--format4 [] = ""
--format4 (x:xs) = format4b x ++ format4 xs

format4b :: [(Int,Int)] -> String
format4b [] = "\n" -- end line onto last pair
format4b [(x,y)] = show x ++ " -- " ++ show y ++ format4b [] -- last element in list
format4b ((x,y):xs) = show x ++ " -- " ++ show y ++ " , " ++ format4b xs -- body of function

-----END #4


------------
--5) test to see if comparator network is a sorting network
t5 :: [(Int, Int)] -> Bool
t5 xs = t3 (reverse $ genlist 1 (length xs-1)) xs == genlist 1 (length xs-1)
--above is equivalent to
--if t3 (reverse $ genlist 1 (length xs)) xs == genlist 1 (length xs) then
--    True
--else
--    False

    -- counter will always be 1 to start with
    --    counter      length
genlist :: Int ->       Int -> [Int]
genlist x y =
    if x == y then
        [x]
    else
        x : genlist (x+1) y

--------------END #5

---------------------
--6)  Generate a sorting network given n number of wires

t6 :: Int -> [[(Int,Int)]]
t6 x =
    let z = filter6 (t6b 1 x) in
        z ++ reverse (init z)


--counter always set to 1 to start
--    count     # wires
t6b :: Int ->    Int -> [[(Int,Int)]] -- processes/creates the entire sorting network
t6b x y =

    if x == y then
        []
    else
        t6c 1 x : t6b (x+1) y

-- counter always starts at 1 for the first wire
--    counter       # wires at this time frame
t6c :: Int ->       Int -> [(Int,Int)] -- processes individual "time" in the sorting network
t6c x y =
    if x == y then
        [(x, x+1)]

    else
        (x,x+1) : t6c (x+1) y

filter6 :: [[(Int,Int)]] -> [[(Int,Int)]]
filter6 [] = []
filter6 [x] = [x]
filter6 (x:y:xs) = x  : filter6 (removeitems6 x y:xs)

removeitems6 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
removeitems6 [] y = y
removeitems6 ((x,y):xs) z =
    if (x,y) `elem` z then
        delete (x,y) (removeitems xs z)
    else
        removeitems6 xs z


