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

Program :: { Program }
Program : AbstrList Exp                                     { Prog $1 $2 }

Stmt :: { Stmt }
Stmt  :  function var '(' varlist ')' maybetype StmtBody    { mkFun $1 $6 $2  (reverse $4) $7 }
      |  reducer  var '(' varlist ')' maybetype '(' ExpBase ')' StmtBody { Language.GaiwanTypes.Reducer $6 $2 (reverse $4)  $8 $10 }

Abstr :: { Abstraction }
Abstr : abstraction var '(' varlist ')' maybetype bracO   pipedStmt bracC  { Language.GaiwanTypes.Abstraction $6 $2 (reverse $4) (reverse $8) }
      | abstraction var '('         ')' maybetype bracO   pipedStmt bracC  { Language.GaiwanTypes.Abstraction $5 $2 [] (reverse $7) }

AbstrList :: {[Abstraction] }
AbstrList : {[] }
          | Abstr AbstrList {$1 : $2 }

pipedStmt :: { [Stmt] }
pipedStmt : Stmt                                            { [$1] }
         | pipedStmt pipe Stmt %prec PIPE                   { $3 : $1 }

StmtBody :: { Exp }
StmtBody : bracO ExpBase bracC  {$2 }

ExpKinds :: { Instr }
ExpKinds : ExpApp                                           { (appToInstr $1) :: Instr }
         | ExpLoop                                          { $1  :: Instr }

ExpApp :: { Exp }
ExpApp : avar '(' explist ')'                               { mkApp $1 (reverse $3) }
       | avar '(' ')'                                       { mkApp $1 [] }

BracExp :: { Exp }
BracExp : bracO ExpBase bracC                               { $2 }

ExpBase :: { Exp }
ExpBase : ExpApp                                            { $1 }
        | ExpBase '%' ExpBase                               { Modulo $1 $3 }
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

typedvar :: { (String, Maybe GBufOrShapeDefault) }
typedvar : var {($1, Nothing) }
         | var ':' type {($1, Just $3) }

maybetype :: { Maybe GBufOrShapeDefault }
maybetype : {- empty -} {Nothing }
          | ':' type {Just $2 }

{- No arrow type needed in the parser -}
type :: { GBufOrShapeDefault }
type : shape {AShape $1 }
     | shape '[' ExpBase ']'  { ABuf $ GaiwanBuf $3 $1 }

shape :: { StmtShape }
shape : int { GaiwanInt }
     | var { if $1 == "int" then GaiwanInt else TVar $1 }
     | tuple '(' shapelist ')'                              {  GaiwanTuple (reverse $3) }

shapelist :: { [StmtShape] }
shapelist : shape {[$1] }
         | shapelist ',' shape { $3 : $1 }

varlist :: { [(String, Maybe GBufOrShapeDefault)] }
varlist : typedvar                                          { [$1] }
        | varlist ',' typedvar                              { $3 : $1 }

explist :: { [Exp] }
explist : ExpBase                                           { [$1] }
        | explist ',' ExpBase                               { $3 : $1 }

stmtList :: { [Stmt] }
stmtList :                                                  { [] }
         | stmtList Stmt                                    { $2 : $1 }
{

mkApp (Var name builtin) = App name builtin

appToInstr  (App name builtin args)  = IApp name builtin args

mkFun :: FunctionType -> (Maybe GBufOrShapeDefault) -> String -> [(String, Maybe GBufOrShapeDefault)] -> Exp -> Stmt
mkFun Language.Tokens.Mapper   =  Language.GaiwanTypes.Mapper
mkFun Language.Tokens.Shaper =  Language.GaiwanTypes.Shaper

parseGaiwan :: String -> Either String Program
parseGaiwan s = runAlex s gaiwanParse
}
