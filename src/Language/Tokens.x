{
module Language.Tokens (
  Token (..),
  Alex, runAlex, parseError, tokenise
  ) where
}

%wrapper "monad"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]    -- alphabetic characters

tokens :-

  $white+        ;
  "--".*        ;
  function                         { ingest $ \s -> TokenFunction }
  let                              { ingest $ \s -> TokenLet }
  in                               { ingest $ \s -> TokenIn }
  $digit+                          { ingest $ \s -> TokenInt (read s) }
  [\=\:\+\-\*\/\(\)\{\}\,\|]       { ingest $ \s -> TokenSym (head s) }
  $alpha [$alpha $digit \_ \']*    { ingest $ \s -> TokenVar s }

{
-- Each action has type :: String -> Token

ingest f (pos, _, _, s) len = return $ f (take len s) pos

-- The token type:
data Token =
  TokenFunction    AlexPosn |
  TokenLet         AlexPosn |
  TokenIn          AlexPosn |
  TokenSym Char    AlexPosn |
  TokenVar String  AlexPosn |
  TokenInt Int     AlexPosn |
  TokenEof
  deriving (Eq,Show)

parseError TokenEof         = alexError $ "Unexpected end of file"
parseError (TokenLet pos)   = alexParseErrorLoc pos $ "Unexpected let"
parseError (TokenSym s pos) = alexParseErrorLoc pos $ "Unexpected symbol " ++ show s
parseError (TokenVar s pos) = alexParseErrorLoc pos $ "Unexpected var " ++ (show s)
parseError (TokenInt i pos) = alexParseErrorLoc pos $ "Unexpected integer " ++ (show i)




alexParseErrorLoc (AlexPn _ line column) msg = alexError $ "parse error at " ++ (show line) ++ ":" ++ (show column) ++ " column: " ++ msg

alexEOF = return TokenEof

tokenise = (alexMonadScan >>=)
}
