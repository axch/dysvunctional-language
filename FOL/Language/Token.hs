{-# LANGUAGE NoImplicitPrelude #-}
module FOL.Language.Token where

import FOL.Language.Common

import Data.Char

data Token
    = TokSymbol String
    | TokReal Real
    | TokBool Bool
    | TokLParen
    | TokRParen
      deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = scan . map toLower

scan :: String -> [Token]
scan [] = []

-- The lexical structure of identifiers follows rather closely that
-- described in Section "7.1.1 Lexical structure" of R5RS.
scan (c:cs)
    | isInitial c
    = TokSymbol (c:cs') : scan cs''
    where
      (cs', cs'') = span isSubsequent cs

-- There are two peculiar identifiers: + and -, which require special
-- care.  They are peculiar in that they may be part of a real.
scan ('+':c:cs)
    | not (isDigitOrDot c)
    = TokSymbol "+" : scan (c:cs)
    | otherwise
    = TokReal (read cs' :: Real) : scan cs''
    where
      (cs', cs'') = scanUnsignedReal (c:cs)
scan ['+'] = [TokSymbol "+"]
scan ('-':c:cs)
    | not (isDigitOrDot c)
    = TokSymbol "-" : scan (c:cs)
    | otherwise
    = TokReal (negate (read cs' :: Real)) : scan cs''
    where
      (cs', cs'') = scanUnsignedReal (c:cs)
scan ['-'] = [TokSymbol "-"]

scan s@(c:_)
    | isDigitOrDot c
    = TokReal (read cs' :: Real) : scan cs''
    where
      (cs', cs'') = scanUnsignedReal s

scan ('#':'t':cs) = TokBool True  : scan cs
scan ('#':'f':cs) = TokBool False : scan cs
scan ('('    :cs) = TokLParen     : scan cs
scan (')'    :cs) = TokRParen     : scan cs

-- We support comments starting with ; and extending to the end of
-- line.
scan (';':cs) = scan (dropWhile (not . isNewline) cs)
scan (c:cs) | isWhitespace c = scan cs

scan _ = error "lexical analysis error"

isInitial :: Char -> Bool
isInitial c = isLetter c || isSpecialInitial c

isSpecialInitial :: Char -> Bool
isSpecialInitial c = c `elem` "!$%&*/:<=>?^_~"

isSubsequent :: Char -> Bool
isSubsequent c = isInitial c || isDigit c || isSpecialSubsequent c

isSpecialSubsequent :: Char -> Bool
isSpecialSubsequent c = c `elem` "+-.@"

scanUnsignedReal :: String -> (String, String)
scanUnsignedReal cs = (integralPart ++ fractionalPart ++ exponentPart, cs''')
    where
      (integralPart,   cs'  ) = scanIntegralPart   cs
      (fractionalPart, cs'' ) = scanFractionalPart cs'
      (exponentPart,   cs''') = scanExponentPart   cs''

scanIntegralPart :: String -> (String, String)
scanIntegralPart cs = (ensureNonEmpty cs', cs'')
    where
      (cs', cs'') = span isDigit cs

scanFractionalPart :: String -> (String, String)
scanFractionalPart ('.':cs) = ('.' : ensureNonEmpty cs', cs'')
    where
      (cs', cs'') = span isDigit cs
scanFractionalPart cs = ([], cs)

scanExponentPart :: String -> (String, String)
scanExponentPart ('e':cs@(c:_))
    | isDigit c
    = ('e' : cs', cs'')
    where
      (cs', cs'') = span isDigit cs
scanExponentPart ('e':'-':cs@(c:_))
    | c == '-'
    = ('e' : '-' : cs', cs'')
    | otherwise
    = error $ "missing number: " ++ cs
    where
      (cs', cs'') = span isDigit cs
scanExponentPart cs       = ([], cs)

ensureNonEmpty :: String -> String
ensureNonEmpty [] = "0"
ensureNonEmpty s  = s

isDot :: Char -> Bool
isDot c = c == '.'

isNewline :: Char -> Bool
isNewline c = c == '\n'

isWhitespace :: Char -> Bool
isWhitespace c = isSpace c || isNewline c

isDigitOrDot :: Char -> Bool
isDigitOrDot c = isDigit c || isDot c
