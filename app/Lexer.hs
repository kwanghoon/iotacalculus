module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

mkFn :: Token -> LexAction Token IO ()   -- (String -> Maybe Token)
mkFn tok = \text -> return $ Just tok

skip :: LexAction Token IO ()            -- String -> Maybe Token
skip = \text -> return Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKENS,
    lexerSpecList = 
      [
        ("[ \t]", skip),   -- skip a space or a tab, not a return!
        ("[\\_a-zA-Z][\\_a-zA-Z0-9]*"    , mkFn IDENTIFIER)
      ]
  }
