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
      return                                                { TokenReturn _ }
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

Program :: { Program String }
Program : AbstrList Exp                                     { Prog $1 $2 }

Stmt :: { Stmt String }
Stmt  :  function var '(' varlist ')' maybetype StmtBody    { mkFun $1 $6 $2  (reverse $4) $7 }
      |  reducer  var '(' varlist ')' maybetype '(' ExpBase ')' StmtBody { Language.GaiwanDefs.Reducer $6 $2 (reverse $4)  $8 $10 }

Abstr :: { Abstraction String }
Abstr : abstraction var '(' varlist ')' maybetype bracO   pipedStmt bracC  { Language.GaiwanDefs.Abstraction $6 $2 (reverse $4) (reverse $8) }
      | abstraction var '('         ')' maybetype bracO   pipedStmt bracC  { Language.GaiwanDefs.Abstraction $5 $2 [] (reverse $7) }

AbstrList :: {[Abstraction String ] }
AbstrList : {[] }
          | Abstr AbstrList {$1 : $2 }

pipedStmt :: { [Stmt String] }
pipedStmt : Stmt                                            { [$1] }
         | pipedStmt pipe Stmt %prec PIPE                   { $3 : $1 }

StmtBody :: { Exp }
StmtBody : bracO ExpBase bracC  {$2 }

ExpKinds :: { Instr }
ExpKinds : ExpApp                                           { $1 :: Instr }
         | ExpLoop                                          { $1  :: Instr }
         | let var '=' bracO Exp bracC
           in bracO Exp bracC                               { (LetB $2 $5 $9) }
         | return var                                       { (Return [$2]) }

ExpApp :: { Instr }
ExpApp : avar '(' explist ')'                               { mkApp $1 (reverse $3) }
       | avar '(' ')'                                       { mkApp $1 [] }


ExpLoop :: {Instr }
ExpLoop : int ':' var  bracO Exp bracC                      { Loop (Int $1) $3 $5 }
        | avar ':' var  bracO Exp bracC                     { Loop $1 $3 $5 }
        | '(' ExpBase ')'  ':' var  bracO Exp bracC         { Loop $2 $5 $7 }

Exp :: {[Instr] }
Exp   : pipedExp                                            { reverse $1 }


avar :: {Exp }
avar : var                                                  { Var $1 False }
     | builtinvar                                           { Var $1 True }

pipedExp :: {[Instr] }
pipedExp : ExpKinds                                         { [$1] }
         | pipedExp pipe ExpKinds %prec PIPE                { $3 : $1 }

typedvar :: { (String, Maybe (GBufOrShape String)) }
typedvar : var {($1, Nothing) }
         | var ':' type {($1, Just $3) }

maybetype :: { Maybe (GBufOrShape String) }
maybetype : {- empty -} {Nothing }
          | ':' type {Just $2 }

{- No arrow type needed in the parser TODO more shapes of the size-}
type :: { (GBufOrShape String) }
type : shape {AShape $1 }
     | shape '[' int '*' var '+' int ']'  { ABuf $ GaiwanBuf (GaiwanBufSize $5 $3 $7) $1 }
     | shape '[' int '*' var ']'  { ABuf $ GaiwanBuf (GaiwanBufSize $5 $3 0) $1 }
     | shape '[' var '+' int ']'  { ABuf $ GaiwanBuf (GaiwanBufSize $3 1 $5) $1 }
     | shape '[' var ']'  { ABuf $ GaiwanBuf (GaiwanBufSize $3 1 0) $1 }

shape :: { GShape String }
shape : int { GaiwanInt }
     | var { if $1 == "int" then GaiwanInt else TVar $1 }
     | tuple '(' shapelist ')'                              {  GaiwanTuple (reverse $3) }

shapelist :: { [GShape String] }
shapelist : shape {[$1] }
         | shapelist ',' shape { $3 : $1 }

varlist :: { [(String, Maybe (GBufOrShape String))] }
varlist : typedvar                                          { [$1] }
        | varlist ',' typedvar                              { $3 : $1 }

explist :: { [Exp] }
explist : ExpBase                                           { [$1] }
        | explist ',' ExpBase                               { $3 : $1 }

BracExp :: { Exp }
BracExp : bracO ExpBase bracC                               { $2 }

ExpBase :: { Exp }
ExpBase : ExpBase '%' ExpBase                               { Modulo $1 $3 }
        | tuple '(' explist ')'                             { Tuple (reverse $3) }
        | ExpBase '[' '[' int  ']' ']'                      { Select $1 $4 }
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
        | let var '=' ExpBase in ExpBase                    { Let $2 $4 $6 }

stmtList :: { [Stmt String] }
stmtList :                                                  { [] }
         | stmtList Stmt                                    { $2 : $1 }
{

mkApp (Var name builtin) = IApp name builtin

mkFun :: FunctionType -> (Maybe (GBufOrShape a)) -> String -> [(String, Maybe (GBufOrShape a))] -> Exp -> Stmt a
mkFun Language.Tokens.Mapper   =  Language.GaiwanDefs.Mapper
mkFun Language.Tokens.Shaper =  Language.GaiwanDefs.Shaper

parseGaiwan :: String -> Either String (Program String)
parseGaiwan s = runAlex s gaiwanParse
}
