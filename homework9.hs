nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' [ e | e <- xs, e /=x ]

isLonger :: [a] -> [b] -> Bool
isLonger [] [] = False
isLonger [] _ = False
isLonger _ [] = True
isLonger (x:xs) (y:ys) = isLonger xs ys

surrounded :: [Int] -> [Int]
surrounded (x:y:[]) = []
surrounded [] = []
surrounded (x:[]) = []
surrounded (x:y:z:xs)
  |x<y && y<z = y : surrounded (y:z:xs)
  |otherwise = surrounded (y:z:xs)

multiple :: Eq a => [a] -> [a]
multiple [] = []
multiple (x:[]) = []
multiple (x:xs)
  |x `elem` xs = nub' (x:(multiple xs))
  |otherwise = nub' (multiple xs)
