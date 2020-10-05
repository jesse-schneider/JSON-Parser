\begin{code}
module Main (main) where
import ABR.Util.Pos
import ABR.Parser

import JsonParser as JS
\end{code}

\noindent Here is our main function to: \\
    - read Input \\
    - prelex the Input into [(Character, Position)] \\
    - Lex the prelex pairs into lexemes \\
    - Parse the output Lexemes \\
    - Display the output or any errors \\


\begin{code}
main :: IO ()
main = do
   json <- readFile "object.json"
   let error :: Pos -> Msg -> IO ()
       error (_,col) msg = do
          putStrLn $ "Error: " ++ msg
          putStrLn json
          let col' = if col < 0
                 then length json
                 else col
          putStrLn $ replicate col' ' '
             ++ "^"
          main
       cps = preLex json
   case inputL cps of
      Error pos msg -> error pos msg
      OK (tlps,_) -> do
         case inputP tlps of
            Error pos msg -> error pos msg
            OK (input,_) -> do
               case input of
                  JS.Json j -> do
                     putStrLn $ "ParseTree: " ++ show j
\end{code}








