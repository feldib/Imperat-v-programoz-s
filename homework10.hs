hasFever :: Int -> Bool
hasFever n
  |((fromIntegral n)-32)/1.8 >= 38 = True
  |otherwise = False

positiveProduct :: (Num a, Ord a) => [a] -> a
positiveProduct [] = 1
positiveProduct (x:xs)
  |x > 0 = x * positiveProduct xs
  |otherwise = positiveProduct xs

mightyGale :: [(String, Int, Int, Int)] -> String
mightyGale [] = ""
mightyGale ((a,b,c,d):xs)
  |c > 110 = a
  |otherwise = mightyGale xs

cipher :: String -> String
cipher [] = []
cipher (x:[]) = []
cipher (x:y:[]) = []
cipher str@(x:y:z:xs)
  |z `elem` "0123456789" = (x:y:[])
  |length [n|n<-str, n `elem` "0123456789"] == 0 = "" 
  |otherwise = cipher (y:z:xs)

pizza :: [(String, Int)] -> Int
pizza [] = 500
pizza ((a,b):xs) = b+(pizza xs)

average :: [Double] -> Double
average ls = sum ls / fromIntegral (length ls)

szavakListaja :: [String] -> Bool
szavakListaja [] = True
szavakListaja (x:[]) = True
szavakListaja (x:y:xs)
  |last x == head y = szavakListaja (y:xs)
  |otherwise = False

validGame :: String -> Bool
validGame ls = szavakListaja (words ls)
  
