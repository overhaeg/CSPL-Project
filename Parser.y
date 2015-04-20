{
module Parser where
import Data.Char
}

%name calc
%tokentype { Token } 
%error { parseError }

%token 
      true             { TokenTrue }
      false            { TokenFalse }
      '0'              { TokenZero }
      if               { TokenIf }
      then             { TokenThen }
      else             { TokenElse }
      succ             { TokenSucc }
      pred             { TokenPred }
      iszero           { TokenIsZero }
      '('              { TokenOB }
      ')'              { TokenCB }
      plus             { TokenPlus }
      min              { TokenMin  }
      mult	       { TokenMult }
      div              { TokenDiv }

%%

Exp   : '(' Exp ')'              { $2 } 	
      | ValBool                  { ValBool $1 }
      | ValNat                   { ValNat $1 }
      | if Exp then Exp else Exp { ExpIf $2 $4 $6 }
      | succ Exp                 { ExpSucc $2 }
      | pred Exp                 { ExpPred $2 }
      | iszero Exp               { ExpIsZero $2 }
      | plus Exp Exp             { ExpPlus $2 $3 }
      | min  Exp Exp             { ExpMin $2 $3 } 
      | mult Exp Exp             { ExpMult $2 $3 }
      | div Exp Exp              { ExpDiv  $2 $3 }

ValBool : false { BoolFalse }
        | true  { BoolTrue }

ValNat : '0' { NatZero }

{

data Exp = ValNat ValNat
         | ValBool ValBool
         | ExpIf Exp Exp Exp
         | ExpSucc Exp
         | ExpPred Exp
         | ExpIsZero Exp
         | ExpPlus Exp Exp
         | ExpMin Exp Exp
	 | ExpMult Exp Exp
	 | ExpDiv Exp Exp
    deriving (Eq, Show)

data ValBool = BoolFalse
             | BoolTrue
    deriving (Eq, Show)    

data ValNat  = NatZero 
    deriving (Eq, Show)

data Token = TokenTrue
           | TokenFalse
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenZero
           | TokenSucc
           | TokenPred
           | TokenIsZero
           | TokenOB
           | TokenCB
           | TokenPlus
           | TokenMin
	   | TokenMult
	   | TokenDiv 
    deriving (Eq, Show)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
lexer ('(':cs) = TokenOB   : lexer cs
lexer (')':cs) = TokenCB   : lexer cs
lexer ('0':cs) = TokenZero : lexer cs
lexer cs = 
    case span isAlpha cs of
        ("true", rest)   -> TokenTrue   : lexer rest
        ("false", rest)  -> TokenFalse  : lexer rest
        ("if", rest)     -> TokenIf     : lexer rest
        ("then", rest)   -> TokenThen   : lexer rest
        ("else", rest)   -> TokenElse   : lexer rest
        ("iszero", rest) -> TokenIsZero : lexer rest
        ("succ", rest)   -> TokenSucc   : lexer rest
        ("pred", rest)   -> TokenPred   : lexer rest
        ("plus", rest)   -> TokenPlus   : lexer rest
        ("min", rest)    -> TokenMin    : lexer rest
	("mult", rest)   -> TokenMult   : lexer rest
	("div", rest)    -> TokenDiv    : lexer rest

parseError :: [Token] -> a
parseError _ = error "Parse error"


}	
        
