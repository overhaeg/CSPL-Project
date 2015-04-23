module Main where

import qualified Parser          as P
import qualified TypeChecker     as T
import qualified Evaluator       as E
import qualified Data.Map.Strict as Map

main = do
    content <- getContents
    let parsed = P.calc . P.lexer $ content
    putStr "Parsed: "
    print parsed
    putStr "Typed:  "
    print . T.checkType Map.empty $ parsed
    putStr "Evaluated: "
    print . E.eval $ parsed
