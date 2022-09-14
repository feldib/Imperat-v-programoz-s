chessTable :: [(Int, Int)]; chessTable = [(a,b) | a<-[0..7], b<-[0..7]]

blackFields :: [(Int, Int)];
blackFields = [(a,b) | a<-[0..7], b<-[0..7], even a && odd b || odd a && even b]