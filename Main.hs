module Main where

import qualified Parser      as P
import qualified TypeChecker as T
import qualified Evaluator   as E

main = do
    content <- getContents
    let parsed = P.calc . P.lexer $ content
    putStr "Parsed: "
    print parsed
    putStr "Typed:  "
    print . T.checkType $ parsed
    putStr "Evaluated: "
    print . E.eval $ parsed
