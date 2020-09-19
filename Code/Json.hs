

data Json = Str String 
    | Number Float
    | Object (Json, Json) 
    | JsonArray [Json] 
    | True 
    | False deriving (Show)