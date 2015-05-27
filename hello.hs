double x = x + x

doubleSmall x = if x > 100
    then x
    else x * 2

lostNumbers = [1,2,5,7,9]
moreLostNumbers = lostNumbers ++ [1,2,3]

-- get first element

firstNumber = head lostNumbers
firstNumber' = lostNumbers !! 0

listOfList = [[1],[2]]

-- check if list is empty
isnotempty = null [1]

reversedlist = reverse [1,2,3]

infiniteEvens = [0,2..]

intsmod3 = cycle [0,1,2]

sevenFives = take 7 (repeat 5)

oddComprehension = [x + 1 | x <- infiniteEvens]

evenFiltered = [x | x <- [0,1..], mod x 2 == 0]


removeNonUppercase :: [Char] -> [Char]
removeNonUppercase s = [c | c <- s, c `elem` ['A'..'Z']]

multiplyList :: [Int] -> Int -> [Int]
multiplyList ls x = [x * y | y <- ls]

multiplyList' :: [Int] -> Int -> [Int]
multiplyList' ls x = map (\y -> y * x) ls

factorial n = product [1..n]

lucky :: Integral a => a -> String
lucky 7 = "Lucky 7!"
lucky x = "Not so lucky :("

factorial' 1 = 1
factorial' x = x * factorial (x - 1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]
-- TODO how can I do this without using concatenation?

first (x, _, _) = x
second (_, x, _) = x
third (_, _, x) = x

--cracklepop :: Integral a [Char] b => [a] -> [b]
divides x y = mod x y == 0

cracklepopn n
    | divides n 15 = "CracklePop"
    | divides n 5 = "Pop"
    | divides n 3 = "Crackle"
    | otherwise = show n

cracklepop :: [Integer] -> IO ()
cracklepop = mapM_ putStrLn . map cracklepopn

cracklepop' :: [Integer] -> IO [()]
cracklepop' = sequence . map putStrLn . map cracklepopn
-- TODO What does the subtle difference in these types mean?


