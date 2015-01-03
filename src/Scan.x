{
{-# OPTIONS_GHC -w #-}
module Scan (scan, Token(..)) where

import Numeric (readSigned, readFloat)
import Text.Read (readMaybe)
import Data.Char (toUpper)
}

%wrapper "basic"
$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

[Rr][Ee][Mm] ([^\n]*) ;
\n { const Newline }
[\ \t]+ ;

($digit)+ { \s -> Int (read s, s) }

($digit)+ \. ($digit)+ { \s -> Dec (fst $ head $ readSigned readFloat s, s) }
\. ($digit)+ { \s -> Dec (fst $ head $ readSigned readFloat $ '0' : s, s) }

\" ([^ \" \\] | (\\ .))* \" { Str . read }
\" ([^ \" \\] | (\\ .))* $ { \s -> Str $ read $ s ++ "\"" }

($alpha)($alpha | $digit)* { Ident }

\= { const Equal }
\< { const Less }
\> { const Greater }
\<\= { const LessEqual }
\=\< { const LessEqual }
\>\= { const GreaterEqual }
\=\> { const GreaterEqual }
\( { const LParen }
\) { const RParen }
\$ { const Dollar }
\: { const Colon }
\; { const Semi }
\# { const Hash }
\+ { const Plus }
\- { const Dash }
\* { const Star }
\/ { const Slash }
\, { const Comma }
\& { const Ampersand }

{

data Token

  = Int (Integer, String)
  | Dec (Rational, String)
  | Str String
  | Newline

  | Ident String

  | Equal
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | LParen
  | RParen
  | Dollar
  | Colon
  | Semi
  | Hash
  | Plus
  | Dash
  | Star
  | Slash
  | Comma
  | Ampersand

  | PRINT
  | COLOR
  | DIM
  | SAY
  | INPUT
  | CALL
  | IF
  | THEN
  | OR
  | AND
  | END
  | LPRINT
  | GOSUB
  | GOTO
  | FOR
  | TO
  | READ
  | NEXT
  | DATA
  | USING
  | RETURN
  | SUB
  | STATIC

  deriving (Eq, Ord, Show, Read)

matchKeywords :: Token -> Token
matchKeywords tok@(Ident s) = case readMaybe $ map toUpper s of
  Just key -> key
  Nothing  -> tok
matchKeywords tok = tok

scan :: String -> [Token]
scan s = map matchKeywords $ alexScanTokens $ s ++ "\n"

}
