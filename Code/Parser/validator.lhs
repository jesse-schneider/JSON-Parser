\begin{code}
module Main (main) where
import ABR.Util.Pos
import ABR.Parser
import JsonParser
import Json
import Data.List
\end{code}



\noindent \textbf{This is our main validation program.}
\begin{code}
main :: IO ()
main = do
   schema <- readFile "schema.json"
   json <- readFile "data.json"
   let error :: Pos -> Msg -> IO ()
       error (_,col) msg = do
          putStrLn $ "Error: " ++ msg
          putStrLn json
          let col' = if col < 0
                 then length json
                 else col
          putStrLn $ replicate col' ' ' ++ "^"
          main
       sps =  preLex schema
       ps = preLex json
       
   case inputL sps of
      Error pos msg -> error pos msg
      OK (slps,_) -> do
        case inputL ps of
            Error pos msg -> error pos msg
            OK (lps,_) -> do
                case inputP slps of
                    Error pos msg -> error pos msg
                    OK (schema,_) -> do
                        case inputP lps of
                            Error pos msg -> error pos msg
                            OK (json,_) -> do
                                case schema of
                                    Json s -> do
                                        putStrLn $ "Schema: " ++
                                         (unlines $ fst $ parseSchema s [] [])
                                        putStrLn $ "Types: " ++ 
                                        (unlines $ snd $ parseSchema s [] [])
                                        case json of
                                            Json j -> do
                                            putStrLn $ "Fields: " ++ 
                                            (unlines $ fst $ parseData j [] [])
                                            putStrLn $ "Values: " ++ 
                                            (unlines $ snd $ parseData j [] [])
                                            if cmpFields (fst $ parseSchema s [] []) 
                                                (fst $ parseData j [] []) then 
                                                putStrLn $ "Validation passed: " ++ 
                                                show (cmpValues (snd $ parseSchema s [] [])
                                                 (snd $ parseData j [] []))
                                                else putStrLn $ "Validation failed"
\end{code}

\newpage
\noindent \textbf{Function to parse the schema ready for validation:}
\begin{code}
parseSchema:: Json -> [String] -> [String] -> ([String], [String])
parseSchema (Object []) ks vs = (ks,vs)
parseSchema (Object (KeyValue (k, Object v):xs)) ks vs = 
    parseSchema (Object (xs ++ v)) (ks ++ [k]) vs
parseSchema (Object (KeyValue (_, String v):xs)) ks vs = 
    parseSchema (Object xs) ks (vs ++ [v])
parseSchema (Object (KeyValue (_, Num v):xs)) ks vs = 
    parseSchema (Object xs) ks (vs ++ [show v])
parseSchema (Object (KeyValue (_, Array v):xs)) ks vs = 
    parseSchema (Object xs) ks (vs ++ [show v])
parseSchema (Object (KeyValue (_, Bool v):xs)) ks vs = 
    parseSchema (Object xs) ks (vs ++ [show v])
\end{code}


\noindent \textbf{Function to parse the data ready for validation:}
\begin{code}
parseData:: Json -> [String] -> [String] -> ([String], [String])
parseData (Object []) ks vs = (ks,vs)
parseData (Object (KeyValue(x1, x2):xs)) ks vs = 
    parseData (Object xs) (ks ++ [x1]) (vs ++ [show $ x2])
parseData (Array x) _ _ = ([], [show (Array x)])
parseData (Num x) _ _ = ([], [show (Num x)])
parseData (String x) _ _ = ([], [show (String x)])
parseData (Bool x) _ _ = ([], [show (Bool x)])
\end{code}


\noindent \textbf{Function to compare field names:}
\begin{code}
cmpFields:: [String] -> [String] -> Bool
cmpFields [] [] = True
cmpFields [f] [k]       = if f == k then True else False
cmpFields (f:fs) (k:ks) | f == "\"schemas\"" = cmpFields fs (k:ks)
                        | f == k            =  cmpFields fs ks
                        | otherwise         = False
\end{code}

\noindent \textbf{Function to compares values/types:}
\begin{code}
cmpValues:: [String] -> [String] -> Bool
cmpValues [f] [k]       | f == "\"string\"" = if (isPrefixOf "String" k) then True 
                         else False
                        | f == "\"int\""    = if (isPrefixOf "Num" k) then True
                         else False
                        | f == "\"float\""  = if (isPrefixOf "Num" k) then True 
                         else False
                        | f == "\"array\""  = if (isPrefixOf "Array" k) then True 
                         else False
                        | f == "\"bool\""  = if (isPrefixOf "Bool" k) then True
                         else False
cmpValues (f:fs) (k:ks) | f == "\"object\"" = cmpValues fs (k:ks)
                        | f == "\"string\"" = if (isPrefixOf "String" k) then cmpValues fs ks
                         else False
                        | f == "\"int\""    = if (isPrefixOf "Num" k) then cmpValues fs ks
                         else False
                        | f == "\"float\""  = if (isPrefixOf "Num" k) then cmpValues fs ks
                         else False
                        | f == "\"array\""  = if (isPrefixOf "Array" k) then cmpValues fs ks
                         else False
                        | f == "\"bool\""  = if (isPrefixOf "Bool" k) then cmpValues fs ks
                         else False
\end{code}






