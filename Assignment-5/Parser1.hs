module Parser1 where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)    
import AS1

{- -----------------------------------------------------------------
   THE GRAMMAR:
     Expr   ::=  Aexpr | Aexpr ? Aexpr : Expr
     Aexpr  ::=  Term {{ + | - } Term}^*
     Term   ::=  Factor {{ * | / } Factor}^*
     Factor ::=  Num | (Expr) 

Note that: 1-2-3-4 = (((1-2)-3)-4), 
           1*2+3 = (1*2)+3,
           1+2*3 = 1+(2*3),
           1?2:3?4:5 = ?2:(3?4:5), 
           etc.
-}

---------------------------------------------------------------------------
-- The parser
---------------------------------------------------------------------------
-- utilities 

run :: ReadP a -> String -> [(a,String)]
run        = readP_to_S
      
full :: ReadP a -> String -> Maybe a
full p inp = listToMaybe [ x | (x,"") <- run p inp]

token :: ReadP a -> ReadP a
token p    = do { result <- p ; skipSpaces ; return result }

symbol :: String -> ReadP String
symbol sym = token (string sym)

parens :: ReadP a -> ReadP a
parens p   = between (symbol "(") (symbol ")") p 

natural :: ReadP Int
natural    = do { ds <- token(munch1 isDigit) ; return (read ds :: Int) }

integer :: ReadP Int
integer    = do { char '-'; n <- natural; return (-n) } <++ natural

-- arithmetic expressions
expr, aexpr, term, factor, numExpr   :: ReadP Aexp
cond                                 :: Aexp -> ReadP Aexp
plusExpr, subExpr, multExpr, divExpr :: ReadP (Aexp -> Aexp -> Aexp)

expr       = do { skipSpaces
                ; e <- aexpr
                ; option e (cond e)
                }
cond tst   = do { symbol "?"
                ; thenExpr <- aexpr
                ; symbol ":"
                ; elseExpr <- expr
                ; return (Cond tst thenExpr elseExpr)
                }

aexpr      = term   `chainl1` (plusExpr <++ subExpr)
term       = factor `chainl1` (multExpr <++ divExpr)
factor     = numExpr <++ parens(expr)

plusExpr   = do { symbol "+"; return Add }
subExpr    = do { symbol "-"; return Sub }
multExpr   = do { symbol "*"; return Mult }
divExpr    = do { symbol "/"; return Div }
numExpr    = do { n <- natural; return (Num (fromIntegral n)) }

aparse :: String -> Aexp
aparse inp = case (full aexpr inp) of
               Nothing   -> error $ "could not parse " ++ inp
               (Just pt) -> pt
