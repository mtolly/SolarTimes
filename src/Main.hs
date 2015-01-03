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
        _ -> error $ "run: undefined; " ++ show stmt

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
