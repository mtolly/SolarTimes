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
  [ (("TRANSLATE", TString), Function $ return . head) -- TODO
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
  Div x y -> do
    vx <- asDouble <$> eval x
    vy <- asDouble <$> eval y
    return $ VDouble $ vx / vy
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

asDouble :: Value -> Double
asDouble v = case v of
  VLong i -> fromIntegral i
  VSingle f -> realToFrac f
  VDouble d -> d
  VString _ -> error "asDouble: got string value"

asString :: Value -> String
asString v = case v of
  VLong i -> show i
  VSingle f -> show f
  VDouble d -> show d
  VString s -> s

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
          modify $ \st -> st
            { bindings = (var, Value v) : bindings st
            }
          run
        Say x -> do
          s <- asString <$> eval x
          liftIO $ putStrLn $ "<< " ++ s ++ " >>"
          run
        _ -> error $ "run: undefined; " ++ show stmt

main :: IO ()
main = do
  stmts <- parseFile . scan <$> readFile "SolarTimes52xx.bas"
  let initialState = BasicState
        { program = stmts
        , current = stmts
        , returnTo = error "Tried to return, but not in a subprogram"
        , bindings = initialFuncs
        , subBindings = []
        }
  evalStateT run initialState
