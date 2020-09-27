
module Main (main) where

import System.IO
import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers
import Data.Char



-- key value pairs to show what is inside a JSON Object
data KeyValue = KeyValue (String, Json) deriving Show

--JSON Data Type
data Json = String String 
            | Num Float
            | Object [KeyValue]
            | Array [Json]
            | Bool Bool deriving (Show)


-- Input Data Type for user input
data Input = Json Json deriving Show


-- boolean literal lexers
trueL :: Lexer
trueL = tokenL "true" %> "true"

falseL :: Lexer
falseL = tokenL "false" %> "false"


-- all symbols to look for when lexing
symbolL :: Lexer
symbolL = literalL '[' <|> literalL ']' 
      <|> literalL '{' <|> literalL '}'
      <|> literalL ':' <|> literalL ','


-- list of lexers to use on input
inputL :: Lexer 
inputL = dropWhite $ nofail $ total $ listL 
    [whitespaceL, floatL, stringL, literalL 'q', symbolL, trueL, falseL]


-- Parsers
inputP :: Parser Input
inputP = nofail $ total (
     jsonP @> Json
    )

-- json value parser
jsonP :: Parser Json
jsonP = 
        tagP "string"
        @> (\(_, x, _) -> String x)
    <|> tagP "float"
        @> (\(_, x, _) -> Num (read x))
    <|> tagP "true"
        @> (\(_, x, _) ->  Bool True )
    <|> tagP "false"
        @> (\(_, x, _) -> Bool False)
    <|> arrayP
        @> (\x -> Array x)
    <|> objectP
        @> (\j -> Object j)


-- array parser
arrayP :: Parser [Json]
arrayP =
    literalP "'['" "["
    <&> optional (
        jsonP
        <&> many (
                    literalP "','" ","
                    &> nofail' "value expected" jsonP
            )
        @> cons
    )
    <& nofail (literalP "']'" "]")
    @> (\((_,_,_), ars) -> concat ars)


-- object parser
objectP :: Parser [KeyValue]
objectP =
    literalP "'{'" "{"
    <&> optional (
            keyValueP
        <&> many (
                literalP "','" ","
                &> nofail' "value expected" keyValueP
            )
        @> cons
    )
    <& nofail (literalP "'}'" "}")
    @> (\((_,_,_), ars) -> concat ars)


-- Key Value Parser
keyValueP :: Parser KeyValue
keyValueP =
    tagP "string"
    <&> nofail (literalP "':'" ":")
    &> nofail' "value expected" jsonP
    @> (\((_,l,_),v) -> KeyValue (l, v))


{- main function to:
    - read in Input
    - prelex the Input into [(Character, Position)]
    - Lex the prelex pairs into lexemes
    - Parse the output Lexemes
    - Display the output or any errors
-}

main :: IO ()
main = do
   putStr "> "
   hFlush stdout
   json <- getLine
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
   putStrLn $ "Pairs: " ++ show cps
   case inputL cps of
      Error pos msg -> error pos msg
      OK (tlps,_)      -> do
         putStrLn $ "Lexemes: " ++ show tlps
         case inputP tlps of
            Error pos msg -> error pos msg
            OK (input,_)    -> do
               putStrLn $ "Input : " 
                  ++ show input
               case input of
                  Json j -> do
                     putStrLn $ "Result: "
                        ++ show j
                     main
