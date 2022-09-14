--1.
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
  | even x = (x:evens xs)
  | otherwise = evens xs

--2.
everySecond :: [a] -> [a]
everySecond [] = []
everySecond (x:[]) = [x]
everySecond (x:xs) = (x:(everySecond (tail xs)))

--3.
howMany :: Char -> String -> Int
howMany _ "" = 0
howMany c (x:xs)
  | c==x = 1 + (howMany c xs)
  | otherwise = howMany c xs
  
 --4.
letterize :: String -> [String]
letterize [] = []
letterize (x:xs) = [x]:(letterize xs)


