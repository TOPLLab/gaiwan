{
module Language.Gaiwan (parseGaiwan, Exp(..), Stmt(..), Program(..)) where
import           Data.Char
import           Data.List
import           Language.Tokens
import           Language.GaiwanDefs
import           Language.GaiwanTypes
}
%name gaiwanParse
%tokentype { Token }
%error { parseError }
%lexer {tokenise } {TokenEof }
%monad {Alex }

%token
      function                                              { TokenFunction $$ _ }
      reducer                                               { TokenReducer  _ }
      abstraction                                           { TokenAbstraction  _ }
      let                                                   { TokenLet      _ }
      in                                                    { TokenIn       _ }
      if                                                    { TokenIf       _ }
      else                                                  { TokenElse       _ }
      tuple                                                 { TokenTuple       _ }
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
      '<'                                                   { TokenSym '>'  _ }
      '>'                                                   { TokenSym '<'  _ }
      '^'                                                   { TokenSym '^'  _ }
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

Stmt  :  function var '(' varlist ')' maybetype StmtBody    { mkFun $1 $6 $2  (reverse $4) $7 }
      |  reducer  var '(' varlist ')' maybetype '(' ExpBase ')' StmtBody { Language.GaiwanTypes.Reducer $6 $2 (reverse $4)  $8 $10 }
      |  abstraction var '(' varlist ')' maybetype bracO   pipedStmt bracC  {Language.GaiwanTypes.Abstraction $6 $2 (reverse $4) $8 }



pipedStmt : Stmt                                            { [$1] }
         | pipedStmt pipe Stmt %prec PIPE                   { $3 : $1 }

StmtBody : bracO ExpBase bracC  {$2 }

ExpKinds : ExpApp                                           { $1 }
         | ExpLoop                                          { $1 }

ExpApp : avar '(' explist ')'                               { mkApp $1 (reverse $3) }
       | avar '(' ')'                                       { mkApp $1 [] }

BracExp : bracO ExpBase bracC                               { $2 }

ExpBase : ExpApp                                            { $1 }
        | ExpBase '%' ExpBase                               { Modulo $1 $3 }
        | tuple '(' explist ')'                             { Tuple (reverse $3) }
        | ExpBase '[' int ']'                               { Select $1 $3 }
        | ExpBase '+' ExpBase                               { Plus $1 $3 }
        | ExpBase '-' ExpBase                               { Minus $1 $3 }
        | ExpBase '*' ExpBase                               { Times $1 $3 }
        | ExpBase '/' ExpBase                               { Div $1 $3 }
        | ExpBase '>' ExpBase                               { IsGreater $1 $3 }
        | ExpBase '<' ExpBase                               { IsGreater $3 $1 }
        | ExpBase '^' ExpBase                               { Pow $1 $3 }
        | '(' ExpBase ')'                                   { $2 }
        | '-' ExpBase %prec NEG                             { Negate $2 }
        | int                                               { Int $1 }
        | ExpBase '[' ExpBase ']'                           { ArrayGet $1 $3 }
        | avar                                              { $1 }
        | if '(' ExpBase ')'  BracExp else BracExp          { If $3 $5 $7 }

ExpLoop : int ':' var  bracO Exp bracC                 { Loop (Int $1) $3 $5 }
        | avar ':' var  bracO Exp bracC                { Loop $1 $3 $5 }
        | '(' ExpBase ')'  ':' var  bracO Exp bracC    { Loop $2 $5 $7 }

Exp   : pipedExp                                            { reverse $1 }

avar : var                                                  { Var $1 False }
     | builtinvar                                           { Var $1 True }

pipedExp : ExpKinds                                         { [$1] }
         | pipedExp pipe ExpKinds %prec PIPE                { $3 : $1 }

typedvar : var {($1, Nothing) :: (String, Maybe GStmtTypeOrShapeDefault) }
         | var ':' type {($1, Just $3) :: (String, Maybe GStmtTypeOrShapeDefault) }

maybetype : {- empty -} {Nothing }
          | ':' type {Just $2 }

{- No arrow type needed in the parser -}
type : shape {AShape $1 }
     | shape '[' ExpBase ']'  { AType $ GaiwanBuf $3 $1 }

shape : int { GaiwanInt }
     | var { if $1 == "int" then GaiwanInt else TVar $1 }
     | tuple '(' shapelist ')'                              {  GaiwanTuple (reverse $3) }

shapelist : shape {[$1] }
         | shapelist ',' shape { $3 : $1 }

typelist : type {[$1] }
         | typelist ',' type { $3 : $1 }

varlist : typedvar                                          { [$1] }
        | varlist ',' typedvar                              { $3 : $1 }

explist : ExpBase                                           { [$1] }
        | explist ',' ExpBase                               { $3 : $1 }

stmtList :                                                  { [] }
         | stmtList Stmt                                    { $2 : $1 }
{
-- | let var '=' Exp in Exp                                 { Let $2 $4 $6 }

mkApp (Var name builtin) = App name builtin

mkFun :: FunctionType -> (Maybe GStmtTypeOrShapeDefault) -> String -> [(String, Maybe GStmtTypeOrShapeDefault)] -> Exp -> Stmt
mkFun Language.Tokens.Mapper   =  Language.GaiwanTypes.Mapper
mkFun Language.Tokens.Shaper =  Language.GaiwanTypes.Shaper

parseGaiwan :: String -> Either String Program
parseGaiwan s = runAlex s gaiwanParse
}
