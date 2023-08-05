module Token (Token(..)) where

import TokenInterface

data Token =
    IDENTIFIER
  | END_OF_TOKENS
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKENS, "$"),
    (IDENTIFIER, "identifier")
  ]

findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

instance TokenInterface Token where
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
      
  isEOT END_OF_TOKENS = True
  isEOT _             = False
    
