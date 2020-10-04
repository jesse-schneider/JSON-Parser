\begin{code}
module JsonParser where
import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers
import Data.Char
import Json
\end{code}

Input Data Type for user input:
\begin{code}
data Input = Json Json deriving Show
\end{code}

\subsection*{Lexers}


Boolean value lexers:
\begin{code}
trueL :: Lexer
trueL = tokenL "true" %> "true"

falseL :: Lexer
falseL = tokenL "false" %> "false"
\end{code}



\noindent Symbol Lexer to find all symbols in JSON:
\begin{code}
symbolL :: Lexer
symbolL = literalL '[' <|> literalL ']' 
      <|> literalL '{' <|> literalL '}'
      <|> literalL ':' <|> literalL ','
\end{code}



\noindent This is a list of Lexers, all the ones we need to use to get JSON Lexemes:
\begin{code}
inputL :: Lexer 
inputL = dropWhite $ nofail $ total $ listL 
    [whitespaceL, floatL, stringL, literalL 'q', symbolL, trueL, falseL]
\end{code}

\newpage

\subsection*{Parsers}


\noindent Our input Parser, parsing our Json Lexemes at the highest level:
\begin{code}
inputP :: Parser Input
inputP = nofail $ total (
     jsonP @> Json
    )
\end{code}


\noindent JSON value Parser, a Parser than can read identify values within JSON:
\begin{code}
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
\end{code}



\noindent JSON Array Parser:
\begin{code}
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
\end{code}



\noindent JSON Object Parser:
\begin{code}
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
    @> (\((_,_,_), kvs) -> concat kvs)
\end{code}



\noindent Object Key Value Pair Parser:
\begin{code}
keyValueP :: Parser KeyValue
keyValueP =
    tagP "string"
    <&> nofail (literalP "':'" ":")
    &> nofail' "value expected" jsonP
    @> (\((_,l,_),v) -> KeyValue (l, v))
\end{code}
