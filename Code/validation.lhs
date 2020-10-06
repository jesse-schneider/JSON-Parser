\subsection*{Passing Validation}
\begin{code}
runhaskell validator.hs 
Schema: "firstName" "lastName" "birthYear"

Types: "object" "string" "string" "int"

Fields: "firstName" "lastName" "birthYear"

Values: String "\"Shirley\""
String "\"Temple\""
Num 1928.0

Validation passed: True
\end{code}


\subsection*{Failing Validation}
\begin{code}
runhaskell validator.hs 
Schema: "firstName" "lastName" "birthYear"

Types: "object" "string" "string" "int"

Fields: "firstName" "lastName" "birthYear"

Values: String "\"Shirley\""
String "\"Temple\""
String "\"1928\""

Validation passed: False
\end{code}
