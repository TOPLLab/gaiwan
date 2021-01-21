{
module Language.Gaiwan (parseGaiwan, Exp(..), Stmt(..), Program(..)) where
import           Data.Char
import           Data.List
import           Language.Tokens
import           Language.GaiwanDefs
}
%name gaiwanParse
%tokentype { Token }
%error { parseError }
%lexer {tokenise } {TokenEof }
%monad {Alex }

%token
      function                                              { TokenFunction $$ _ }
      let                                                   { TokenLet      _ }
      in                                                    { TokenIn       _ }
      int                                                   { TokenInt $$   _ }
      var                                                   { TokenVar $$   _ }
      builtinvar                                            { TokenBuildinVar $$   _ }
      '='                                                   { TokenSym '='  _ }
      ':'                                                   { TokenSym ':'  _ }
      semi                                                  { TokenSym ';'  _ }
      '+'                                                   { TokenSym '+'  _ }
      '-'                                                   { TokenSym '-'  _ }
      '*'                                                   { TokenSym '*'  _ }
      '/'                                                   { TokenSym '/'  _ }
      '%'                                                   { TokenSym '%'  _ }
      '('                                                   { TokenSym '('  _ }
      ')'                                                   { TokenSym ')'  _ }
      bracO                                                 { TokenSym '{'  _ }
      bracC                                                 { TokenSym '}'  _ }
      '['                                                   { TokenSym '['  _ }
      ']'                                                   { TokenSym ']'  _ }
      ','                                                   { TokenSym ','  _ }
      pipe                                                  { TokenSym '|'  _ }

%right in
%left PIPE
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG
%%

Program : stmtList Exp                                      { Prog (reverse $1) $2 }

Stmt  :  function var '(' varlist ')' bracO ExpBaseL bracC  { mkFun $1 $2  (reverse $4) (reverse $7) }
      |  function var '('         ')' bracO ExpBaseL bracC  { mkFun $1 $2  [] (reverse $6) }

ExpKinds : ExpApp                                           { $1 }
         | ExpLoop                                          { $1 }

ExpApp : avar '(' explist ')'                               { mkApp $1 (reverse $3) }
       | avar '(' ')'                                       { mkApp $1 [] }


ExpBaseL : ExpBase                                          { [$1] }
         | ExpBaseL semi ExpBase %prec PIPE                 { $3:$1 }

ExpBase : ExpApp                                            { $1 }
        | ExpBase '%' ExpBase                               { Modulo $1 $3 }
        | ExpBase '+' ExpBase                               { Plus $1 $3 }
        | ExpBase '-' ExpBase                               { Minus $1 $3 }
        | ExpBase '*' ExpBase                               { Times $1 $3 }
        | ExpBase '/' ExpBase                               { Div $1 $3 }
        | '(' ExpBase ')'                                   { $2 }
        | '-' ExpBase %prec NEG                             { Negate $2 }
        | int                                               { Int $1 }
        | avar                                              { $1 }

ExpLoop : int ':' var  bracO pipedExp bracC                 { Loop $1 $3 $5 }

Exp   : pipedExp                                            { cleanPiped (reverse $1) }

avar : var                                                  { Var $1 False }
     | builtinvar                                           { Var $1 True }

pipedExp : ExpKinds                                         { [$1] }
         | pipedExp pipe ExpKinds %prec PIPE                { $3 : $1 }

varlist : var                                               { [$1] }
        | varlist ',' var                                   { $3 : $1 }

explist : ExpBase                                           { [$1] }
        | explist ',' ExpBase                               { $3 : $1 }

stmtList :                                                  { [] }
         | stmtList Stmt                                    { $2 : $1 }
{
-- | let var '=' Exp in Exp                                 { Let $2 $4 $6 }

-- Simplify a list of piped expression (remove pipe if only one)
cleanPiped [x] = x
cleanPiped x   = PipedExp x

mkApp (Var name builtin) = App name builtin

mkFun :: FunctionType -> String -> [String] -> [Exp] -> Stmt
mkFun Language.Tokens.Mapper   =  Language.GaiwanDefs.Mapper
mkFun Language.Tokens.Shuffler =  Language.GaiwanDefs.Shuffler

parseGaiwan :: String -> Either String Program
parseGaiwan s = runAlex s gaiwanParse
}
