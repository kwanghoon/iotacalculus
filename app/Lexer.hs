module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token
import qualified Data.Map as Map

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
        ("[ \t\n]", skip),   -- skip a space, a tab, and a return!
        ("\\/\\/[^\n]*", skip),  -- skip a line comment starting with //

        ("\"[^\"]*\"" , mkFn STRINGLITERAL),
        ("([0-9]*[.])?[0-9]+" , mkFn NUMBERLITERAL),

        ("\\[", mkFn OPENBRACKET),
        ("\\]", mkFn CLOSEBRACKET),
        ("\\{", mkFn OPENBRACE),
        ("\\}", mkFn CLOSEBRACE),
        ("\\(", mkFn OPENPARENTHESIS),
        ("\\)", mkFn CLOSEPARENTHESIS),

        ("\\.", mkFn DOT),
        ("\\,", mkFn COMMA),
        ("\\;", mkFn SEMICOLON),
        ("~>", mkFn EVENTARROW),
        ("->", mkFn GROUPARROW),

        ("\\|\\|", mkFn OR),
        ("\\&\\&", mkFn AND),
        ("==", mkFn EQUAL),
        ("!=", mkFn NOTEQUAL),
        ("<", mkFn LESSTHAN),
        ("<=", mkFn LESSTHANOREQUAL),
        (">", mkFn GREATERTHAN),
        (">=", mkFn GREATERTHANOREQUAL),

        ("~", mkFn NEG),
        ("\\+", mkFn ADDITION),
        ("\\-", mkFn SUBTRACTION),
        ("\\*", mkFn MULTIPLICATION),
        ("\\/", mkFn DIVISION),

        (":=", mkFn ASSIGN),

        ("[\\_a-zA-Z][\\_a-zA-Z0-9]*"    , keywordOrIdentifier)
      ]
  }

keywordMap = Map.fromList (map swap keywords)
  where swap (a,b) = (b,a)

keywordOrIdentifier text = 
  case Map.lookup text keywordMap of
    Nothing -> return $ Just IDENTIFIER
    Just tok -> return $ Just tok
