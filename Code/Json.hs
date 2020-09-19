

data Json = Str String 
    | Number Float
    | Object (String, Json) 
    | Array [Json] 
    | True 
    | False deriving (Show)