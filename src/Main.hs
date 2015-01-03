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
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Bits

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
      return $ VSingle $ fromInteger $ floor $ asDouble arg
    )
  , ( ("FIX", TSingle)
    , Function $ \[arg] ->
      return $ VSingle $ fromInteger $ truncate $ asDouble arg
    )
  , ( ("STR", TString)
    , Function $ \[arg] -> return $ VString $ case arg of
      VString s -> s
      VLong   i -> if i <= 0 then show i else ' ' : show i
      VSingle f -> if f <= 0 then show f else ' ' : show f
      VDouble d -> if d <= 0 then show d else ' ' : show d
    )
  , ( ("CHR", TString)
    , Function $ \[arg] -> return $ VString [toEnum $ fromIntegral $ asLong arg]
    )
  , ( ("ABS", TSingle)
    , Function $ \[arg] -> return $ VSingle $ realToFrac $ abs $ asDouble arg
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
  Compare ord x y -> do
    vx <- asDouble <$> eval x
    vy <- asDouble <$> eval y
    return $ VLong $ if compare vx vy == ord then -1 else 0
  Var v -> case v of
    FuncArray fun args -> do
      bin <- getBinding fun
      case bin of
        Value _ -> error $ "eval: tried to call a non-function/array " ++ show fun
        Array vals -> case args of
          [arg] -> do
            dbl <- asDouble <$> eval arg
            return $ vals !! round dbl
          _ -> error $ "eval: tried to access array with more than 1 index"
        Function f -> mapM eval args >>= f
    SimpleVar sv -> do
      bin <- getBinding sv
      case bin of
        Value  val -> return val
        Array    _ -> error $ "eval: tried to evaluate array "    ++ show sv
        Function _ -> error $ "eval: tried to evaluate function " ++ show sv
  _ -> error $ "eval: undefined; " ++ show e
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
  VString s -> read s

asLong :: Value -> Int32
asLong v = case v of
  VLong   i -> i
  VSingle f -> round f
  VDouble d -> round d
  VString s -> read s

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
          liftIO $ putStr "[[ "
          forM_ xs $ \x -> eval x >>= liftIO . putStr . asString
          liftIO $ putStr " ]]"
          liftIO $ putChar '\n'
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
          when b $ modify $ \st -> st
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
        _ -> error $ "run: undefined; " ++ show stmt

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
          case splitAt index xs of
            (before, _ : after) -> let
              xs' = before ++ [val'] ++ after
              in modify $ \st -> st
                { bindings = (fun, Array xs') : bindings st
                }
            (_, []) -> error "assign: tried to assign to too-large array index"
        _ -> error "assign: tried to assign to array using more than 1 index"
      _ -> undefined

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  stmts <- parseFile . scan <$> readFile "SolarTimes52xx.bas"
  let initialState = BasicState
        { program = stmts
        , current = stmts
        , returnTo = error "Tried to return, but not in a subprogram"
        , bindings = initialFuncs
        , subBindings = []
        }
  evalStateT run initialState
