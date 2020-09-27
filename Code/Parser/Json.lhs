\begin{code}
module Json where
\end{code}

\subsection*{JSON Data Types}

\noindent Key Value pair data type to show what is inside a JSON Object:
\begin{code}
data KeyValue = KeyValue (String, Json) deriving Show
\end{code}


\noindent JSON Data Type:
\begin{code}
data Json = String String 
            | Num Float
            | Object [KeyValue]
            | Array [Json]
            | Bool Bool deriving (Show)
\end{code}