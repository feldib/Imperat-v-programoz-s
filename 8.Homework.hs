isLonger :: [a] -> [b] -> Bool
isLonger [] [] = False
isLonger [] _ = False
isLonger _ [] = True
isLonger (x:xs) (y:ys) = isLonger xs ys

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs)
  |x<head xs = isSorted xs
  |otherwise = False

fromTo :: Int -> Int -> [a] -> [a]
fromTo 0 0 _ = []
fromTo a b (x:xs)
  |a<0 = fromTo 0 b (x:xs)
  |a>0 && b<0 = []
  |a>b = []
  |otherwise = drop a(take b (x:xs))
