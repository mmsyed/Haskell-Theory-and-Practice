-- Mo Syed mmsyed@syr.edu

import AS1
import Parser1 hiding(run)
import Debug.Trace

------------------------------------------------------------------------
-- Contents
--   fixMe - a stub
--   eval  - a simple evaluator for the L1 language
--   run   - given a string version of an arthmetic expression,
--             run parse it, evaluated it, and prints the answer.
--   rep   - similar to run except it prompts you for the expression. 
------------------------------------------------------------------------


fixMe = error "Please fix me"
eror = error "Divided by zero error"

-- eval e | trace ("entering eval with arg: "++ show e) False = undefined 
eval (Num n)          = n
eval (Add a1 a2)      = (eval a1) + (eval a2)
eval (Sub a1 a2)      = (eval a1) - (eval a2)
eval (Mult a1 a2)     = (eval a1) * (eval a2)
eval (Div a1 a2)      = if (eval a2 == 0) then (eror) else (div (eval a1) (eval a2))
eval (Cond a1 a2 a3 ) = if (eval a1 ==0) then (eval a3) else (eval a2)

------------------------------------------------------------------------
-- run e 
--   parses e, evaluates e, prints the answer
--   Try: (run "2+3*5")
run :: String -> IO ()
run etxt = do { let e = aparse etxt
              ; putStrLn $ "Evaluating: " ++ show e
              ; let val = eval e
              ; putStrLn $ "    Result: " ++ show val
              }

-- read-eval-print
--   Try: rep
--   and type 2+3*5 to the Expression?-prompt
rep :: IO ()
rep = do { putStr     "Expression?  \t"
         ; etxt     <- getLine;
         ; let e    =  aparse etxt
         ; let val  =  eval e
         ; putStrLn $ "Evaluates to:\t" ++ show val
         }

