{
module Language.Gaiwan (parseGaiwan, Exp, Stmt, Program) where
import Data.Char
import Data.List
import Language.Tokens
}
%name gaiwanParse
%tokentype { Token }
%error { parseError }
%lexer {tokenise} {TokenEof}
%monad {Alex}

%token
      function        { TokenFunction _ }
      let             { TokenLet      _ }
      in              { TokenIn       _ }
      int             { TokenInt $$   _ }
      var             { TokenVar $$   _ }
      '='             { TokenSym '='  _ }
      ':'             { TokenSym ':'  _ }
      '+'             { TokenSym '+'  _ }
      '-'             { TokenSym '-'  _ }
      '*'             { TokenSym '*'  _ }
      '/'             { TokenSym '/'  _ }
      '('             { TokenSym '('  _ }
      ')'             { TokenSym ')'  _ }
      '{'             { TokenSym '{'  _ }
      '}'             { TokenSym '}'  _ }
      ','             { TokenSym ','  _ }
      pipe            { TokenSym '|'  _ }

%right in
%left PIPE
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG
%%

Program : stmtList Exp                      { Prog (reverse $1) $2 }

Stmt  :  function var '(' varlist ')' '{' Exp '}'    { Fun $2  $4 $7 }

ExpBase
      : let var '=' Exp in Exp              { Let $2 $4 $6 }
      | var '(' explist ')'                 { App (Var $1) $3 }
      | ExpBase '+' ExpBase                 { Plus $1 $3 }
      | ExpBase '-' ExpBase                 { Minus $1 $3 }
      | ExpBase '*' ExpBase                 { Times $1 $3 }
      | ExpBase '/' ExpBase                 { Div $1 $3 }
      | '(' Exp ')'                         { $2 }
      | '-' Exp %prec NEG                   { Negate $2 }
      | int                                 { Int $1 }
      | var                                 { Var $1 }

Exp   : pipedExp                            { PipedExp (reverse $1) }

varlist : {- empty -}            { [] }
        | varlist var            { $2 : $1 }

explist : {- empty -}            { [] }
        | explist Exp            { $2 : $1 }


stmtList : {- empty -}    {[]}
         | stmtList Stmt  { $2 : $1 }


pipedExp : ExpBase {[$1]}
         | pipedExp pipe ExpBase %prec PIPE { $3 : $1 }


{
data Program
      = Prog [Stmt] Exp
      deriving Show

data Stmt
      = Fun String [String] Exp
      deriving Show

data Exp
      = Let String Exp Exp
      | Plus Exp Exp
      | Minus Exp Exp
      | App Exp [Exp]
      | Times Exp Exp
      | Div Exp Exp
      | Int Int
      | Var String
      | Brack Exp
      | Negate Exp
      | PipedExp [Exp]
      deriving Show

parseGaiwan :: String -> Either String Program
parseGaiwan s = runAlex s gaiwanParse
}
