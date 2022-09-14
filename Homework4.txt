aWeekStudents :: String; aWeekStudents = ['A'..'K']

bWeekStudents :: String; bWeekStudents = ['L'..'Z']

olimpics :: [Int]; olimpics = [2000,2004..3000]

mountain :: Integral a => a -> [a]; mountain a = if a==0; then []; else if a==1; then [1]; else [1..a] ++ [(a-1),(a-2)..1]