{
module Language.Tokens (
  Token (..),
  FunctionType (..),
  Alex, runAlex, parseError, tokenise
  ) where
}

%wrapper "monad"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]    -- alphabetic characters

tokens :-

  $white+        ;
  "--".*        ;
  mapper                           { ingest $ \s -> TokenFunction Mapper }
  shuffler                         { ingest $ \s -> TokenFunction Shuffler }
  let                              { ingest $ \s -> TokenLet }
  in                               { ingest $ \s -> TokenIn }
  $digit+                          { ingest $ \s -> TokenInt (read s) }
  [\%\=\:\+\-\*\/\(\)\{\}\,\|]       { ingest $ \s -> TokenSym (head s) }
  $alpha [$alpha $digit \_ \']*    { ingest $ \s -> TokenVar s }
  @ [$alpha $digit \_ \']+    { ingest $ \s -> TokenBuildinVar (tail s) }

{
-- Each action has type :: String -> Token

ingest f (pos, _, _, s) len = return $ f (take len s) pos

data FunctionType = Mapper | Shuffler deriving (Eq,Ord,Enum,Show)

-- The token type:
data Token =
  TokenFunction FunctionType    AlexPosn |
  TokenLet              AlexPosn |
  TokenIn               AlexPosn |
  TokenSym Char         AlexPosn |
  TokenVar String       AlexPosn |
  TokenBuildinVar String       AlexPosn |
  TokenInt Int          AlexPosn |
  TokenEof
  deriving (Eq,Show)

parseError TokenEof         = alexError $ "Unexpected end of file"
parseError (TokenLet pos)   = alexParseErrorLoc pos $ "Unexpected let"
parseError (TokenSym s pos) = alexParseErrorLoc pos $ "Unexpected symbol " ++ show s
parseError (TokenVar s pos) = alexParseErrorLoc pos $ "Unexpected var " ++ (show s)
parseError (TokenBuildinVar s pos) = alexParseErrorLoc pos $ "Unexpected build in var " ++ (show s)
parseError (TokenInt i pos) = alexParseErrorLoc pos $ "Unexpected integer " ++ (show i)
parseError (TokenFunction t pos) = alexParseErrorLoc pos $ "Unexpected " ++ (show t) ++ " function "




alexParseErrorLoc (AlexPn _ line column) msg = alexError $ "parse error at " ++ (show line) ++ ":" ++ (show column) ++ " column: " ++ msg

alexEOF = return TokenEof

tokenise = (alexMonadScan >>=)
}
