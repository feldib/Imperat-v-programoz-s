import Data.List

morseTab :: [(Char, String)]
morseTab =
  [('A',".-"),('B',"-..."),('C',"-.-."),('D',"-.."),('E',".")
  ,('F',"..-."),('G',"--."),('H',"...."),('I',".."),('J',".---")
  ,('K',"-.-"),('L',".-.."),('M',"--"),('N',"-."),('O',"---")
  ,('P',".--."),('Q',"--.-"),('R',".-."),('S',"..."),('T',"-")
  ,('U',"..-"),('V',"...-"),('W',".--"),('X',"-..-")
  ,('Y',"-.--"),('Z',"--..")
  ,('0',"-----"),('1',".----"),('2',"..---"),('3',"...--"),('4',"....-")
  ,('5',"....."),('6',"-...."),('7',"--..."),('8',"---.."),('9',"----.")
  ]
  
normalizeText :: String -> String
normalizeText [] = []
normalizeText (x:xs)
 |x `elem ` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" = x:(normalizeText xs)
 |otherwise = normalizeText xs
 
charToCode :: [(Char, String)] -> Char -> String
charToCode ((a,b):xs) char
  |char == a = b
  |otherwise = charToCode xs char

encodeToWords :: String -> [String]
encodeToWords "" = []
encodeToWords (x:xs) = (charToCode morseTab x):(encodeToWords xs)

encodeString :: String -> String
encodeString "" = ""
encodeString str = filter (/=' ') (unwords (encodeToWords str))

codeToChar :: [(a,String)] -> String -> a
codeToChar ((a,b):xs) char
  |char == b = a
  |otherwise = codeToChar xs char

decodeWords :: [String] -> String
decodeWords [] = ""
decodeWords (x:xs) = (codeToChar morseTab x):(decodeWords xs)

getPossiblePrefixes :: [(Char,String)] -> String -> [(Char,String)]
getPossiblePrefixes x "" = []
getPossiblePrefixes [] _ = []
getPossiblePrefixes ((a,b):xs) str@(y:[])
  |b `Data.List.isPrefixOf` str = [(a,b)]
  |otherwise = getPossiblePrefixes xs str
getPossiblePrefixes ((a,b):xs) str
  |b `Data.List.isPrefixOf` str = ((a,b):(getPossiblePrefixes xs str))
  |otherwise = getPossiblePrefixes xs str
  
--withShortestCodes :: [(Char,String)] -> [Char]

--withShortestCodes ((a,b):(c,d):xs)
--  |(length b)<(length d) = withShortestCodes ((a,b):xs)
--  |(length b)>(length d) = withShortestCodes ((c,d):xs)
--  |(length b)==(length d) = withShortestCodes ((a,b):(c,d):[withShortestCodes xs])
