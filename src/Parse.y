{
module Parse
( parseFile
, parseLine
, Stmt(..)
, Expr(..)
, Var(..)
, SimpleVar
, Type(..)
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
  -- '\n' { Newline }

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

Line
  : int Stmts { Label $1 : $2 }
  | Stmts { $1 }

Stmts
  : { [] }
  | Stmt { [$1] }
  | Stmt ':' Stmts { $1 : $3 }

Stmt
  : PRINT SemiExprs { Print $2 }
  | PRINT USING Expr ';' SemiExprs { PrintUsing $3 $5 }
  | LPRINT SemiExprs { LPrint $2 }
  | LPRINT USING Expr ';' SemiExprs { LPrintUsing $3 $5 }
  | COLOR int ',' int { Color $2 $4 }
  | DIM Var { Dim $2 }
  | SimpleVar '=' Expr { Assign $1 $3 }
  | SAY Expr { Say $2 }
  | INPUT Expr ';' Vars { Input (Just $2) $4 }
  | INPUT Vars { Input Nothing $2 }
  | CALL ident '(' SimpleVars ')' { Call $2 $4 }
  | IF Expr THEN Stmt { If $2 $4 }
  | IF Expr THEN { StartIf $2 }
  | END IF { EndIf }
  | GOTO Expr { Goto $2 }
  | GOSUB Expr { Gosub $2 }
  | FOR SimpleVar '=' Expr TO Expr { For $2 $4 $6 }
  | NEXT SimpleVar { Next $2 }
  | READ Var { Read $2 }
  | DATA Args { Data $2 }
  | END { End }
  | RETURN { Return }
  | SUB ident '(' SimpleVars ')' STATIC { Sub $2 $4 }
  | END SUB { EndSub }

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
  : ExprMod '+' ExprAdd { Add $1 $3 }
  | ExprMod '-' ExprAdd { Subtract $1 $3 }
  | ExprMod { $1 }

ExprMod
  : ExprMult { $1 }

ExprMult
  : ExprUMinus '*' ExprMult { Mult $1 $3 }
  | ExprUMinus '/' ExprMult { Div $1 $3 }
  | ExprUMinus { $1 }

ExprUMinus
  : '-' ExprUMinus { Subtract (Double 0) $2 }
  | Expr0 { $1 }

Expr0
  : '(' Expr ')' { $2 }
  | dec '#' { Double $ realToFrac $1 }
  | dec { Double $ realToFrac $1 }
  | int { Double $ fromIntegral $1 }
  | str { String $1 }
  | Var { Var $1 }

Args
  : { [] }
  | Expr { [$1] }
  | Expr ',' Args { $1 : $3 }

Var
  : SimpleVar { SimpleVar $1 }
  | SimpleVar '(' Args ')' { FuncArray $1 $3 }

SimpleVar
  : ident { ($1, TSingle) }
  | ident '$' { ($1, TString) }
  | ident '#' { ($1, TDouble) }
  | ident '&' { ($1, TLong) }

Vars
  : { [] }
  | Var { [$1] }
  | Var ',' Vars { $1 : $3 }

SimpleVars
  : { [] }
  | SimpleVar { [$1] }
  | SimpleVar ',' SimpleVars { $1 : $3 }

SemiExprs
  : { [] }
  | Expr { [$1] }
  | Expr ';' SemiExprs { $1 : $3 }

int : intfull { fst $1 }
dec : decfull { fst $1 }

{

data Stmt
  = Print [Expr]
  | PrintUsing Expr [Expr]
  | LPrint [Expr]
  | LPrintUsing Expr [Expr]
  | Color Integer Integer
  | Dim Var
  | Assign SimpleVar Expr
  | Say Expr
  | Input (Maybe Expr) [Var]
  | Call String [SimpleVar]
  | If Expr Stmt
  | Goto Expr
  | Gosub Expr
  | StartIf Expr
  | EndIf
  | For SimpleVar Expr Expr
  | Next SimpleVar
  | Read Var
  | Data [Expr]
  | End
  | Return
  | Sub String [SimpleVar]
  | EndSub
  | Label Integer
  deriving (Eq, Ord, Show, Read)

data Expr
  = Double Double
  | String String
  | Var Var
  | Compare Ordering Expr Expr
  | Not Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Or Expr Expr
  | And Expr Expr
  | Add Expr Expr
  | Subtract Expr Expr
  deriving (Eq, Ord, Show, Read)

data Var
  = SimpleVar SimpleVar
  | FuncArray SimpleVar [Expr]
  deriving (Eq, Ord, Show, Read)

type SimpleVar = (String, Type)

data Type
  = TSingle
  | TDouble
  | TLong
  | TString
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

parseFile :: [Token] -> [Stmt]
parseFile = concatMap parseLine . filter (not . null) . splitOn [Newline]

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
