import Data.List (group)

segedF1 :: [Char] -> (Int, Char)
segedF1 str = ((length str), (str!!0))

compress :: [Char] ->  [(Int, Char)]
compress str = map segedF1 (group str)

segedF2 :: (Int, Char) -> [Char]
segedF2 (a, b) = replicate a b

decompress :: [(Int, Char)] -> [Char]
decompress x = concat (map segedF2 x)
