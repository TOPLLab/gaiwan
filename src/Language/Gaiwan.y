{
module Language.Gaiwan (parseGaiwan, Exp(..), Stmt(..), Program(..)) where
import           Data.Char
import           Data.List
import           Language.Tokens
}
%name gaiwanParse
%tokentype { Token }
%error { parseError }
%lexer {tokenise} {TokenEof}
%monad {Alex}

%token
      function        { TokenFunction $$ _ }
      let             { TokenLet      _ }
      in              { TokenIn       _ }
      int             { TokenInt $$   _ }
      var             { TokenVar $$   _ }
      builtinvar      { TokenBuildinVar $$   _ }
      '='             { TokenSym '='  _ }
      ':'             { TokenSym ':'  _ }
      '+'             { TokenSym '+'  _ }
      '-'             { TokenSym '-'  _ }
      '*'             { TokenSym '*'  _ }
      '/'             { TokenSym '/'  _ }
      '%'             { TokenSym '%'  _ }
      '('             { TokenSym '('  _ }
      ')'             { TokenSym ')'  _ }
      bracO           { TokenSym '{'  _ }
      bracC           { TokenSym '}'  _ }
      '['             { TokenSym '['  _ }
      ']'             { TokenSym ']'  _ }
      ','             { TokenSym ','  _ }
      pipe            { TokenSym '|'  _ }

%right in
%left PIPE
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG
%%

Program : stmtList Exp                                      { Prog (reverse $1) $2            }

Stmt  :  function var '(' varlist ')' bracO Exp bracC       { mkFun $1 $2  (reverse $4) $7    }
      |  function var '('         ')' bracO Exp bracC       { mkFun $1 $2  [] $6              }

ExpBase : let var '=' Exp in Exp                            { Let $2 $4 $6                    }
      | avar '(' explist ')'                            { mkApp $1 (reverse $3)             }
      | avar '(' ')'                                    { mkApp $1 []                       }
      | ExpBase '%' ExpBase                                 { Modulo $1 $3                    }
      | ExpBase '+' ExpBase                                 { Plus $1 $3                      }
      | ExpBase '-' ExpBase                                 { Minus $1 $3                     }
      | ExpBase '*' ExpBase                                 { Times $1 $3                     }
      | ExpBase '/' ExpBase                                 { Div $1 $3                       }
      | '(' Exp ')'                                         { $2                              }
      | '-' Exp %prec NEG                                   { Negate $2                       }
      | int                                                 { Int $1                          }
      | avar                                                 { $1                     }

Exp   : pipedExp                                            { cleanPiped (reverse $1)         }
avar : var                                                  { Var $1 False                    }
     | builtinvar                                           { Var $1 True                     }

pipedExp : ExpBase                                          { [$1]                            }
         | pipedExp pipe ExpBase %prec PIPE                 { $3 : $1                         }
varlist : var                                               { [$1]                            }
        | varlist ',' var                                   { $3 : $1                         }
explist : Exp                                               { [$1]                            }
        | explist ',' Exp                                   { $3 : $1                         }
stmtList :                                                  { []                              }
         | stmtList Stmt                                    { $2 : $1                         }
{
data Program
      = Prog [Stmt] Exp
      deriving Show

data Stmt
      = Mapper String [String] Exp
      | Shuffler String [String] Exp
      deriving Show

data Exp
      = Let String Exp Exp
      | Plus Exp Exp
      | Minus Exp Exp
      | App String Bool [Exp]
      | Modulo Exp Exp
      | Times Exp Exp
      | Div Exp Exp
      | Int Int
      | Var String Bool
      | Negate Exp
      | PipedExp [Exp]
      | ArrayGet Exp Exp
      deriving Show

-- Simplify a list of piped expression (remove pipe if only one)
cleanPiped [x] = x
cleanPiped x   = PipedExp x

mkApp (Var name builtin) = App name builtin

mkFun :: FunctionType -> String -> [String] -> Exp -> Stmt
mkFun Language.Tokens.Mapper   =  Language.Gaiwan.Mapper
mkFun Language.Tokens.Shuffler =  Language.Gaiwan.Shuffler

parseGaiwan :: String -> Either String Program
parseGaiwan s = runAlex s gaiwanParse
}
