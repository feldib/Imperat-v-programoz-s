isSmallPrime :: Int -> Bool
isSmallPrime n = n `elem` [2,3,5,7]

equivalent :: Bool -> Bool -> Bool
equivalent True True = True
equivalent True False = False
equivalent False True = False
equivalent False False = True

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = ((a*d+c*b),(b*d))

multiply :: (Int, Int) -> (Int, Int) -> (Int, Int)
multiply (a,b) (c,d) = ((a*c),(b*d))

divide :: (Int, Int) -> (Int, Int) -> (Int, Int)
divide (a,b) (c,d) = ((a*d),(b*c))
