{
module Parse
( parseFile
, parseLine
) where

import Scan
import Data.List.Split
}

%name parseLine
%tokentype { Token }
%error { parseError }

%token
  intfull { Int $$ }
  decfull { Dec $$ }
  str { Str $$ }
  '\n' { Newline }

  ident { Ident $$ }

  '=' { Equal }
  '<' { Less }
  '>' { Greater }
  '<=' { LessEqual }
  '>=' { GreaterEqual }
  '(' { LParen }
  ')' { RParen }
  '$' { Dollar }
  ':' { Colon }
  ';' { Semi }
  '#' { Hash }
  '+' { Plus }
  '-' { Dash }
  '*' { Star }
  '/' { Slash }
  ',' { Comma }
  '&' { Ampersand }

  PRINT { PRINT }
  COLOR { COLOR }
  DIM { DIM }
  SAY { SAY }
  INPUT { INPUT }
  CALL { CALL }
  IF { IF }
  THEN { THEN }
  OR { OR }
  AND { AND }
  END { END }
  LPRINT { LPRINT }
  GOSUB { GOSUB }
  GOTO { GOTO }
  FOR { FOR }
  TO { TO }
  READ { READ }
  NEXT { NEXT }
  DATA { DATA }
  USING { USING }
  RETURN { RETURN }
  SUB { SUB }
  STATIC { STATIC }

%%

Line : int Stmts { ($1, $2) }

Stmts
  : { [] }
  | Stmt { [$1] }
  | Stmt ':' Stmts { $1 : $3 }

Stmt
  : PRINT { Print "" }
  | PRINT str { Print $2 }
  | COLOR int ',' int { Color $2 $4 }
  | DIM ident '(' int ')' { Dim $2 $4 }
  | Var '=' Expr { Assign $1 $3 }
  | SAY Expr { Say $2 }
  | INPUT Expr ';' Var { Input $2 $4 }
  | CALL ident '(' Vars ')' { Call $2 $4 }
  | IF Expr THEN Stmt { If $2 $4 }
  | IF Expr THEN { StartIf $2 }
  | END IF { EndIf }
  | GOTO Expr { Goto $2 }

Expr
  : ExprOr { $1 }

ExprOr
  : ExprAnd OR ExprOr { Or $1 $3 }
  | ExprAnd { $1 }

ExprAnd
  : ExprCmp AND ExprAnd { And $1 $3 }
  | ExprCmp { $1 }

ExprCmp
  : ExprAdd '<' ExprAdd { Compare LT $1 $3 }
  | ExprAdd '=' ExprAdd { Compare EQ $1 $3 }
  | ExprAdd '>' ExprAdd { Compare GT $1 $3 }
  | ExprAdd '<=' ExprAdd { Not $ Compare GT $1 $3 }
  | ExprAdd '>=' ExprAdd { Not $ Compare LT $1 $3 }
  | ExprAdd { $1 }

ExprAdd
  : ExprMod { $1 }

ExprMod
  : ExprMult { $1 }

ExprMult
  : Expr0 '*' ExprMult { Mult $1 $3 }
  | Expr0 '/' ExprMult { Div $1 $3 }
  | Expr0 { $1 }

Expr0
  : '(' Expr ')' { $2 }
  | dec '#' { Double $ realToFrac $1 }
  | dec { Double $ realToFrac $1 }
  | int { Double $ fromIntegral $1 }
  | str { String $1 }
  | Var { Var $1 }
  | Var '(' Args ')' { Func $1 $3 }

Args
  : { [] }
  | Args1 { $1 }

Args1
  : Expr { [$1] }
  | Expr ',' Args1 { $1 : $3 }

Var
  : ident { ($1, TSingle) }
  | ident '$' { ($1, TString) }
  -- | ident '!' { ($1, TSingle) }
  | ident '#' { ($1, TDouble) }
  -- | ident '%' { ($1, TShort) }
  | ident '&' { ($1, TLong) }

Vars
  : { [] }
  | Vars1 { $1 }

Vars1
  : Var { [$1] }
  | Var ',' Vars1 { $1 : $3 }

int : intfull { fst $1 }
dec : decfull { fst $1 }

{

type Line = (Integer, [Stmt])

data Stmt
  = Print String
  | Color Integer Integer
  | Dim String Integer
  | Assign Var Expr
  | Say Expr
  | Input Expr Var
  | Call String [Var]
  | If Expr Stmt
  | Goto Expr
  | StartIf Expr
  | EndIf
  deriving (Eq, Ord, Show, Read)

data Expr
  = Double Double
  | Func Var [Expr]
  | String String
  | Var Var
  | Compare Ordering Expr Expr
  | Not Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Or Expr Expr
  | And Expr Expr
  deriving (Eq, Ord, Show, Read)

type Var = (String, Type)

data Type
  = TSingle
  | TDouble
  | TShort
  | TLong
  | TString
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

parseFile :: [Token] -> [Line]
parseFile = map parseLine . filter (not . null) . splitOn [Newline]

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
