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
      '('              { TokenOP }
      ')'              { TokenCP }
      '['              { TokenOB }
      ']'              { TokenCB }
      plus             { TokenPlus }
      min              { TokenMin  }
      mult	           { TokenMult }
      div              { TokenDiv }
      lambda           { TokenLambda }
      forall           { TokenForAll }
      def              { TokenDef    }
      '->'             { TokenTypeArrow }
      '=>'             { TokenKindArrow } 
      ':'              { TokenColon }
      '::'             { TokenDoubleColon } 
      '.'              { TokenDot } 
      '*'              { TokenKind }
      var              { TokenVar $$ }
      tvar             { TokenTVar $$}
      Bool     	       { TokenBool }
      Nat              { TokenNat }
%%

Exp   : '(' Exp ')'                           { $2 } 	
      | ValBool                               { ValBool $1 }
      | ValNat                                { ValNat $1 }
      | if Exp then Exp else Exp              { ExpIf $2 $4 $6 }
      | succ Exp                              { ExpSucc $2 }
      | pred Exp                              { ExpPred $2 }
      | iszero Exp                            { ExpIsZero $2 }
      | plus Exp Exp                          { ExpPlus $2 $3 }
      | min  Exp Exp                          { ExpMin $2 $3 } 
      | mult Exp Exp                          { ExpMult $2 $3 }
      | div Exp Exp                           { ExpDiv  $2 $3 }
      | var				                      { ExpVar $1 }
      |	'(' lambda var ':' Types '.' Exp ')'  { ExpLambda $3 $5 $7 }
      | '(' lambda tvar '::' Kinds '.' Exp ')' { ExpTAbs $3 $5 $7 }
      | '(' Exp Exp ')'  			          { ExpApp $2 $3 }	
      | '[' Types ']'                         { ExpTPar $2 }


Types : Nat                              { TypeNat }
      | Bool                             { TypeBool }
      | tvar                             { TypeVar $1 }
      | '(' TypeComp ')'                 { $2 }

TypeComp : Types '->' Types                 { TypeArrow $1 $3}
         | forall tvar '::' Kinds '.' Types { TypeUniv $2 $4 $6 }
         | lambda tvar '::' Kinds '.' Types { TypeOpAbs $2 $4 $6 }
         | Types Types                      { TypeOpApp $1 $2 }

Kinds : '*'                      { KindStar }
      | '(' Kinds '=>' Kinds ')'  { KindArrow $2 $4 }

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
         | ExpVar String
         | ExpLambda String Type Exp
         | ExpTAbs String Kind Exp
         | ExpApp Exp Exp
         | ExpTPar Type
    deriving (Eq, Show)

data Type = TypeNat
	  | TypeBool
	  | TypeArrow Type Type
      | TypeVar String
      | TypeUniv String Kind Type
      | TypeOpAbs String Kind Type
      | TypeOpApp Type Type
    deriving (Eq, Show)

data Kind = KindStar
          | KindArrow Kind Kind
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
           | TokenOP
           | TokenCP
           | TokenOB
           | TokenCB
           | TokenPlus
           | TokenMin
	       | TokenMult
	       | TokenDiv
	       | TokenLambda
           | TokenDef
           | TokenForAll
	       | TokenTypeArrow
           | TokenKindArrow
	       | TokenColon
           | TokenDoubleColon
           | TokenDot
           | TokenKind
           | TokenVar String
           | TokenTVar String
	       | TokenBool
           | TokenNat
    deriving (Eq, Show)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
lexer ('(':cs)     = TokenOP          : lexer cs
lexer (')':cs)     = TokenCP          : lexer cs
lexer ('[':cs)     = TokenOB          : lexer cs
lexer (']':cs)     = TokenCB          : lexer cs
lexer ('0':cs)     = TokenZero        : lexer cs
lexer ('=':'>':cs) = TokenKindArrow   : lexer cs
lexer ('-':'>':cs) = TokenTypeArrow   : lexer cs
lexer (':':cs)     = TokenColon       : lexer cs
lexer (':':':':cs) = TokenDoubleColon : lexer cs
lexer ('*':cs)     = TokenKind        : lexer cs
lexer ('.':cs)     = TokenDot         : lexer cs
lexer cs = 
    case span isLower cs of
        ("true", rest)   -> TokenTrue    : lexer rest
        ("false", rest)  -> TokenFalse   : lexer rest
        ("if", rest)     -> TokenIf      : lexer rest
        ("then", rest)   -> TokenThen    : lexer rest
        ("else", rest)   -> TokenElse    : lexer rest
        ("iszero", rest) -> TokenIsZero  : lexer rest
        ("succ", rest)   -> TokenSucc    : lexer rest
        ("pred", rest)   -> TokenPred    : lexer rest
        ("plus", rest)   -> TokenPlus    : lexer rest
        ("min", rest)    -> TokenMin     : lexer rest
        ("mult", rest)   -> TokenMult    : lexer rest
        ("div", rest)    -> TokenDiv     : lexer rest
        ("lambda", rest) -> TokenLambda  : lexer rest
        ("def", rest)    -> TokenDef     : lexer rest
        ("forall", rest) -> TokenForAll  : lexer rest
        ("", rest)       -> case span isAlpha cs of
                                ("Bool", rest)   -> TokenBool      : lexer rest
                                ("Nat", rest)    -> TokenNat       : lexer rest
                                (tvar, rest)     -> TokenTVar tvar : lexer rest
        (var, rest)      -> TokenVar var : lexer rest

parseError :: [Token] -> a
parseError e = error $ "Parse error: " ++ show e


}	
        
