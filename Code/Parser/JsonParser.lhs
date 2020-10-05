\begin{code}
module JsonParser where
import ABR.Parser
import ABR.Parser.Lexers
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
    [whitespaceL, floatL, stringL, symbolL, trueL, falseL]
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
        @> (\(_,x,_) -> String x)
    <|> tagP "float"
        @> (\(_,x,_) -> Num (read x))
    <|> tagP "true"
        @> (\(_,_, _) ->  Bool True )
    <|> tagP "false"
        @> (\(_,_,_) -> Bool False)
    <|> literalP "'['" "[" &> arrayP
        @> (\x -> Array x)
    <|> literalP "'{'" "{" &> objectP
        @> (\x -> Object x)
\end{code}



\noindent JSON Array Parser:
\begin{code}
arrayP :: Parser [Json]
arrayP = optional (
            jsonP
            <&> many (
                    literalP "','" ","
                    &> nofail' "json value expected" jsonP
            )
        @> cons
    )
    <& nofail (literalP "']'" "]")
    @> (\ars -> concat ars)
\end{code}



\noindent JSON Object Parser:
\begin{code}
objectP :: Parser [KeyValue]
objectP = optional (
            keyValueP
            <&> many (
                    literalP "','" ","
                    &> nofail' "json value expected" keyValueP
                )
            @> cons
        )
        <& nofail (literalP "'}'" "}")
        @> (\kvs -> concat kvs)
\end{code}



\noindent Object Key Value Pair Parser:
\begin{code}
keyValueP :: Parser KeyValue
keyValueP =
    tagP "string"
    <&> nofail (literalP "':'" ":")
    &> nofail' "json value expected" jsonP
    @> (\((_,l,_),v) -> KeyValue (l, v))
\end{code}