f1 :: Int -> Bool
f1 n
  |n `mod` 2==0||n `mod` 3==0||n `mod` 5==0 = True
  |otherwise = False

matchingArgs :: Eq a => a -> a -> a -> Bool 
matchingArgs a b c
  |a==b = True
  |a==c = True
  |b==c = True
  |otherwise = False

head2 :: [a] -> (a,a)
head2 (a:b:xs) = (a,b)

increaseEvens :: [Int] -> [Int]
increaseEvens [] = []
increaseEvens (x:xs)
  |x `mod` 2 == 0 = (x+1):(increaseEvens xs)
  |otherwise = x:(increaseEvens xs)

--sameSign :: [Int] -> Bool
--sameSign [] = True
--sameSign ls@(x:y:xs)
-- |x==0 = sameSign (y:xs)
-- |length(ls)==3 && x == 0 = sameSign (y:xs)
-- |length(ls)==3 && y == 0 = sameSign (x:xs)
-- |length(ls)==3 && ls!!(length(ls)-1) == 0 = sameSign (take 2 ls)
-- |length(ls)==2 && x>0 && y>0 = True
-- |length(ls)==2 && x<0 && y<0 = True
-- |x>0 && y>0 = sameSign (y:xs)
-- |x<0 && y<0 = sameSign (y:xs)
-- |otherwise = False

upperWords :: String -> [String]
upperWords ls = [szo|szo<-szavak, 'A' `elem` szo|| 'B' `elem` szo|| 'C' `elem` szo|| 'D' `elem` szo|| 'E' `elem` szo|| 'F' `elem` szo|| 'G' `elem` szo|| 'H' `elem` szo|| 'I' `elem` szo|| 'J' `elem` szo|| 'K' `elem` szo|| 'L' `elem` szo|| 'M' `elem` szo|| 'N' `elem` szo|| 'O' `elem` szo|| 'P' `elem` szo|| 'Q' `elem` szo|| 'R' `elem` szo|| 'S' `elem` szo || 'T' `elem` szo|| 'U' `elem` szo|| 'V' `elem` szo|| 'W' `elem` szo|| 'X' `elem` szo|| 'Y' `elem` szo|| 'Z' `elem` szo]
  where szavak = words ls

ownSum :: [Int] -> Int
ownSum (x:[]) = x
ownSum (x:[y]) = sum (x:((-1)*y):[])
ownSum (x:y:xs) = sum (x:((-1)*y):(ownSum xs):[])  

isPartOf :: Eq a => [a] -> [a] -> Bool
isPartOf [] [] = True
isPartOf [] _ = True
isPartOf x@(x1:xs) y@(y1:ys) 
 |length x > length y = False
 |length x == length y && x1 /= y1 = False
 |length x == length y && length x == 1 && x1 == y1 = True
 |length x == 1 && x1 == y1 = True
 |length x == 1 && x1 /= y1 = isPartOf x ys
 |x1 == y1 = isPartOf xs ys
 |x1 /= y1 = isPartOf x ys
 
--removeHyphens :: String -> String
--removeHyphens "" = ""
--removeHyphens ls(x:xs)
--   |x=='-' = removeHyphens xs
--   |otherwise = (x:(removeHyphens xs))
 
 --removeMiddleHyphens :: String -> String
 --removeMiddleHyphens ls@(x:xs)
 --  |x=='-' = removeMiddleHyphens xs
 
--replaceAt :: Int -> a -> [a] -> [a]
--replaceAt 0 x [] = [x]
--replaceAt 




hanySzamjegy :: Int -> Int
hanySzamjegy n
  |n>=0 && n<10 = 1
  |n>=0 = 1 + (hanySzamjegy (n `div` 10))
  |n<0 && n>(-10) = 1
  |n<0 = 1 + (hanySzamjegy (n `div` 10))
  
szamjegyreBontas :: Int -> [Int]
szamjegyreBontas 0 = []
szamjegyreBontas n = mod n 10 : szamjegyreBontas (div n 10)

narcissistic :: Int -> Bool
narcissistic n
  |sum [s^(hanySzamjegy n) | s<- (szamjegyreBontas n)] == n = True
  |otherwise = False
  
toroldki ::  Eq a => a-> [a] -> [a]
toroldki x [] = []
toroldki x (y:ys)
  |x/=y = y:(toroldki x ys)
  |x==y = ys
  
elevate :: Eq a => a -> [a] -> [a]
elevate _ [] = []
elevate x ls
  |not (x `elem` ls) = ls
  |length ls == 1 && ls!!0 == x = [x]
  |otherwise = x:(toroldki x ls)
