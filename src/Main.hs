module Main where

import Scan
import Parse
import Control.Applicative
import Data.Int
import Text.Show.Functions ()
import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import qualified System.IO as IO
import Data.Bits
import Text.Printf (printf)
import Text.Read (readMaybe)

data Value
  = VLong Int32
  | VSingle Float
  | VDouble Double
  | VString String
  deriving (Eq, Ord, Show, Read)

data BasicState = BasicState
  { program :: [Stmt]
  , current :: [Stmt]
  , returnTo :: [Stmt]
  , bindings :: [(SimpleVar, Binding)]
  , subBindings :: [(SimpleVar, SimpleVar)]
  , dataValues :: [Expr]
  , loops :: [(SimpleVar, ([Value], [Stmt]))]
  , linePrint :: IO.Handle
  } deriving (Show)

type Basic = StateT BasicState IO

data Binding
  = Value Value
  | Array [Value]
  | Function ([Value] -> Basic Value)
  deriving (Show)

initialFuncs :: [(SimpleVar, Binding)]
initialFuncs =
  [ ( ("TRANSLATE", TString)
    , Function $ \[arg] -> return arg
    ) -- TODO
  , ( ("INT", TSingle)
    , Function $ \[arg] ->
      return $ VLong $ floor $ asDouble arg
    )
  , ( ("FIX", TSingle)
    , Function $ \[arg] ->
      return $ VLong $ truncate $ asDouble arg
    )
  , ( ("CINT", TSingle)
    , Function $ \[arg] ->
      return $ VLong $ round $ asDouble arg
    )
  , ( ("CLNG", TSingle)
    , Function $ \[arg] ->
      return $ VLong $ round $ asDouble arg
    )
  , ( ("VAL", TSingle)
    , Function $ \[arg] ->
      return $ VSingle $ realToFrac $ asDouble arg
    )
  , ( ("STR", TString)
    , Function $ \[arg] -> return $ VString $ let
      showNum n = let
        prefix = if n < 0 then "" else " "
        simple = show n
        fixed = case reverse simple of
          '0' : '.' : rest -> reverse rest
          _ -> simple
        in prefix ++ fixed
      in case arg of
        VString s -> s
        VLong   i -> showNum i
        VSingle f -> showNum f
        VDouble d -> showNum d
    )
  , ( ("CHR", TString)
    , Function $ \[arg] -> return $ VString [toEnum $ fromIntegral $ asLong arg]
    )
  , ( ("ABS", TSingle)
    , Function $ \[arg] -> return $ VSingle $ realToFrac $ abs $ asDouble arg
    )
  , ( ("SGN", TSingle)
    , Function $ \[arg] -> return $ VSingle $ realToFrac $ signum $ asDouble arg
    )
  , ( ("COS", TSingle)
    , Function $ \[arg] -> return $ VSingle $ realToFrac $ cos $ asDouble arg
    )
  , ( ("SIN", TSingle)
    , Function $ \[arg] -> return $ VSingle $ realToFrac $ sin $ asDouble arg
    )
  , ( ("ATN", TSingle)
    , Function $ \[arg] -> return $ VSingle $ realToFrac $ atan $ asDouble arg
    )
  , ( ("SQR", TSingle)
    , Function $ \[arg] -> return $ VSingle $ realToFrac $ sqrt $ asDouble arg
    )
  , ( ("LEN", TSingle)
    , Function $ \[arg] -> return $ VLong $ fromIntegral $ length $ asString arg
    )
  , ( ("RIGHT", TString)
    , Function $ \[str, int] -> return $ VString $
      reverse $ take (fromIntegral $ asLong int) $ reverse $ asString str
    )
  ] -- TODO

getBinding :: SimpleVar -> Basic Binding
getBinding sv = do
  sbins <- gets subBindings
  bins <- gets bindings
  let sv' = fromMaybe sv $ lookup sv sbins
  case lookup sv' bins of
    Nothing  -> error $ "getBinding: undefined binding " ++ show sv'
    Just bin -> return bin

eval :: Expr -> Basic Value
eval e = case e of
  String s -> return $ VString s
  Double d -> return $ VDouble d
  Div x y -> math (/) x y
  Mult x y -> math (*) x y
  Add x y -> math (+) x y
  Subtract x y -> math (-) x y
  Or x y -> bitwise (.|.) x y
  And x y -> bitwise (.&.) x y
  Compare EQ x y -> do
    vx <- eval x
    vy <- eval y
    return $ case (vx, vy) of
      (VString sx, VString sy) -> VLong $ if sx == sy then -1 else 0
      _ -> VLong $ if asDouble vx == asDouble vy then -1 else 0
  Compare ord x y -> do
    vx <- asDouble <$> eval x
    vy <- asDouble <$> eval y
    return $ VLong $ if compare vx vy == ord then -1 else 0
  Not x -> do
    vx <- asLong <$> eval x
    return $ VLong $ complement vx
  Var v -> case v of
    FuncArray fun args -> do
      bin <- getBinding fun
      case bin of
        Value _ -> error $ "eval: tried to call a non-function/array " ++ show fun
        Array vals -> case args of
          [arg] -> do
            dbl <- asDouble <$> eval arg
            return $ vals !! (round dbl - 1)
          _ -> error $ "eval: tried to access array with more than 1 index"
        Function f -> mapM eval args >>= f
    SimpleVar sv -> do
      bin <- getBinding sv
      case bin of
        Value  val -> return val
        Array    _ -> error $ "eval: tried to evaluate array "    ++ show sv
        Function _ -> error $ "eval: tried to evaluate function " ++ show sv
  where math op x y = do
          vx <- asDouble <$> eval x
          vy <- asDouble <$> eval y
          return $ VDouble $ op vx vy
        bitwise op x y = do
          vx <- asLong <$> eval x
          vy <- asLong <$> eval y
          return $ VLong $ op vx vy

asDouble :: Value -> Double
asDouble v = case v of
  VLong   i -> fromIntegral i
  VSingle f -> realToFrac f
  VDouble d -> d
  VString s -> case readMaybe s of
    Nothing -> error $ "asDouble: couldn't read as double; " ++ show s
    Just res -> res

asLong :: Value -> Int32
asLong v = case v of
  VLong   i -> i
  VSingle f -> round f
  VDouble d -> round d
  VString s -> case readMaybe s of
    Nothing -> error $ "asLong: couldn't read as long; " ++ show s
    Just res -> res

asBool :: Value -> Bool
asBool = (/= 0) . asDouble

asString :: Value -> String
asString v = case v of
  VLong i -> show i
  VSingle f -> show f
  VDouble d -> show d
  VString s -> s

castTo :: Type -> Value -> Value
castTo TLong v = VLong $ case v of
  VSingle x -> round x
  VDouble x -> round x
  VLong   x -> x
  VString x -> read x
castTo TString v = VString $ case v of
  VSingle x -> show x
  VDouble x -> show x
  VLong   x -> show x
  VString x -> x
castTo TSingle v = VSingle $ case v of
  VSingle x -> x
  VDouble x -> realToFrac x
  VLong   x -> fromIntegral x
  VString x -> read x
castTo TDouble v = VDouble $ case v of
  VSingle x -> realToFrac x
  VDouble x -> x
  VLong   x -> fromIntegral x
  VString x -> read x

isSubStart :: String -> Stmt -> Bool
isSubStart s (Sub s' _) = s == s'
isSubStart _ _          = False

run :: Basic ()
run = do
  stmts <- gets current
  case stmts of
    [] -> return ()
    stmt : rest -> do
      modify $ \st -> st { current = rest }
      case stmt of
        Label _ -> run
        Print xs -> do
          forM_ xs $ \x -> eval x >>= liftIO . putStr . asString
          liftIO $ putChar '\n'
          run
        LPrint xs -> do
          h <- gets linePrint
          forM_ xs $ \x -> eval x >>= liftIO . IO.hPutStr h . asString
          liftIO $ IO.hPutChar h '\n'
          run
        Color _ _ -> run -- TODO
        Dim var -> case var of
          FuncArray simp [Double len] -> do
            let initVal = case snd simp of
                  TSingle -> VSingle 0
                  TDouble -> VDouble 0
                  TLong   -> VLong   0
                  TString -> VString ""
            modify $ \st -> st
              { bindings = (simp, Array $ replicate (round len) initVal) : bindings st
              }
            run
          _ -> error $ "Unsupported DIM statement: " ++ show stmt
        Assign var x -> do
          v <- eval x
          assign (SimpleVar var) v
          run
        Say x -> do
          s <- asString <$> eval x
          liftIO $ putStrLn $ "<< " ++ s ++ " >>"
          run
        Input prompt vars -> do
          case prompt of
            Nothing -> return ()
            Just expr -> eval expr >>= liftIO . putStr . asString
          liftIO $ putStr "? "
          input <- liftIO getLine
          forM_ (zip vars $ splitOn "," input) $ \(var, str) ->
            assign var $ VString str
          run
        Call s svs -> do
          subStart <- dropWhile (not . isSubStart s) <$> gets program
          case subStart of
            Sub _ formals : theSub -> do
              modify $ \st -> st
                { current = theSub
                , returnTo = current st
                , subBindings = zip formals svs
                }
              run
            _ -> error $ "run: couldn't find subprogram " ++ s
        EndSub -> do
          modify $ \st -> st
            { current = returnTo st
            , subBindings = []
            }
          run
        If cond body -> do
          b <- asBool <$> eval cond
          modify $ \st -> if b
            then st
              { current = body : current st
              }
            else st
              { current = dropWhile (not . isLabel) $ current st
              }
          run
        StartIf cond -> do
          b <- asBool <$> eval cond
          unless b $ modify $ \st -> st
            { current = dropWhile (not . (== EndIf)) $ current st
            }
          run
        EndIf -> run
        Gosub expr -> do
          line <- asLong <$> eval expr
          modify $ \st -> st
            { current = dropWhile (not . isLabelFor (fromIntegral line)) $ program st
            , returnTo = current st
            }
          run
        Goto expr -> do
          line <- asLong <$> eval expr
          modify $ \st -> let
            jumpTo = dropWhile (not . isLabelFor (fromIntegral line)) $ program st
            in st
              { current = jumpTo
              , dataValues = [ x | Data exprs <- jumpTo, x <- exprs ]
              }
          run
        Return -> do
          modify $ \st -> st
            { current = returnTo st
            }
          run
        Read var -> do
          exprs <- gets dataValues
          case exprs of
            [] -> error "run: tried to read but out of data"
            x : xs -> do
              eval x >>= assign var
              modify $ \st -> st
                { dataValues = xs
                }
              run
        For sv x y -> do
          vx <- asLong <$> eval x
          vy <- asLong <$> eval y
          assign (SimpleVar sv) $ VLong vx
          modify $ \st -> st
            { loops = (sv, (map VLong [vx + 1 .. vy], current st)) : loops st
            }
          run
        Next sv -> do
          ls <- gets loops
          case lookup sv ls of
            Nothing -> error $ "run: couldn't find loop variable " ++ show sv
            Just (vals, next) -> case vals of
              [] -> do
                modify $ \st -> st
                  { loops = filter ((/= sv) . fst) $ loops st
                  }
                run
              v : vs -> do
                assign (SimpleVar sv) v
                modify $ \st -> st
                  { current = next
                  , loops = (sv, (vs, next)) : loops st
                  }
                run
        End -> return ()
        Data _ -> run
        PrintUsing format exprs -> do
          s <- asString <$> eval format
          vals <- mapM eval exprs
          liftIO $ putStr $ formatUsing s vals
          run
        LPrintUsing format exprs -> do
          s <- asString <$> eval format
          vals <- mapM eval exprs
          h <- gets linePrint
          liftIO $ IO.hPutStr h $ formatUsing s vals
          run
        Sub _ _ -> run

formatUsing :: String -> [Value] -> String
formatUsing "###.#"  = concatMap $ printf "%5.1f"  . asDouble
formatUsing "####.#" = concatMap $ printf "%6.1f"  . asDouble
formatUsing " ###.#" = concatMap $ printf " %5.1f" . asDouble
formatUsing f = error $ "formatUsing: unrecognized format string; " ++ show f

isLabel :: Stmt -> Bool
isLabel (Label _) = True
isLabel _         = False

isLabelFor :: Integer -> Stmt -> Bool
isLabelFor i (Label i') = i == i'
isLabelFor _ _          = False

assign :: Var -> Value -> Basic ()
assign (SimpleVar sv) val = do
  sbins <- gets subBindings
  let sv' = fromMaybe sv $ lookup sv sbins
  modify $ \st -> st
    { bindings = (sv', Value $ castTo (snd sv') val) : bindings st
    }
assign (FuncArray fun args) val = do
  bins <- gets bindings
  let val' = castTo (snd fun) val
  case lookup fun bins of
    Nothing -> error "assign: tried to assign to undefined array"
    Just bin -> case bin of
      Array xs -> case args of
        [arg] -> do
          index <- round . asDouble <$> eval arg
          case splitAt (index - 1) xs of
            (before, _ : after) -> let
              xs' = before ++ [val'] ++ after
              in modify $ \st -> st
                { bindings = (fun, Array xs') : bindings st
                }
            (_, []) -> error $
              "assign: tried to assign to too-large array index; " ++ show (index, xs)
        _ -> error "assign: tried to assign to array using more than 1 index"
      _ -> undefined

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  stmts <- parseFile . scan <$> readFile "SolarTimes52xx.bas"
  IO.withFile "lineprint.txt" IO.WriteMode $ \hnd -> do
    let initialState = BasicState
          { program = stmts
          , current = stmts
          , returnTo = error "Tried to return, but not in a subprogram"
          , bindings = initialFuncs
          , subBindings = []
          , dataValues = [ expr | Data exprs <- stmts, expr <- exprs ]
          , loops = []
          , linePrint = hnd
          }
    evalStateT run initialState
