-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad
import Data.Maybe (maybeToList)
import Data.Bits (Bits(xor))

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return x = Comp (const (Right x, []) )
  (>>=) :: Comp a -> (a -> Comp b) -> Comp b
  comp0 >>= f = Comp {runComp = \env -> let (either, results) = runComp comp0 env in
   case either of
     Left runError -> (Left runError, results)
     Right a -> let (either2, r') = runComp (f a) env in
      case either2 of
        Left e -> (Left e , results ++ r')
        Right a -> (Right a, results ++ r')
      }

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
--  todo 这里是否需要在第二个参数里加“runError”??
abort :: RunError -> Comp a
abort runError = Comp {runComp = const (Left runError, ["runError"])}

getBinding :: Env -> VName -> Value
getBinding env v = foldr (\x acc -> if fst x == v then snd x else acc) NoneVal env

isVarInEnv :: Env -> VName -> Bool
isVarInEnv env v = foldr (\x acc -> (fst x == v) || (acc || False)) False env

look :: VName -> Comp Value
look v = Comp {runComp = \env -> if (isVarInEnv env v)
  then
    (Right (getBinding env v), [])
  else
    (Left (EBadVar v), [] )
  }


addVarToEnv :: VName -> Value -> Env -> Env
addVarToEnv v value env = if isVarInEnv env v
  then
    filter (\(key, _) -> key /= v) env ++ [(v, value)]
  else
    env ++ [(v, value)]

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding v value comp0 = Comp {runComp = \env ->
  let newEnv = addVarToEnv v value env in runComp comp0 newEnv
  }


output :: String -> Comp ()
output s = Comp{runComp = \env -> (Right (), [s])}

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal  = False
truthy TrueVal = True
truthy FalseVal = False
truthy (IntVal 0) = False
truthy (IntVal x) = True
truthy (StringVal "") = False
truthy (StringVal s) = True
truthy (ListVal []) = False
truthy (ListVal l) = True


--  todo 补充剩余的
operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal x) (IntVal y) = Right (IntVal (x + y))
operate Minus (IntVal x) (IntVal y) = Right (IntVal (x - y))
operate Times (IntVal x) (IntVal y) = Right (IntVal (x * y))
operate Div (IntVal x) (IntVal y) = Right (IntVal (x `div` y))
operate Mod (IntVal x) (IntVal y) = Right (IntVal (x `mod` y))

operate Eq (IntVal x) (IntVal y) = Right (if x == y then TrueVal else FalseVal)
operate Eq TrueVal FalseVal = Right FalseVal
operate Eq TrueVal TrueVal = Right TrueVal
operate Eq FalseVal FalseVal = Right TrueVal
operate Eq FalseVal TrueVal = Right FalseVal
operate Eq NoneVal NoneVal = Right TrueVal
operate Eq (StringVal x) (StringVal y) = Right (if x == y then TrueVal else FalseVal)
operate Eq (ListVal x) (ListVal y) = Right (if x == y then TrueVal else FalseVal)

operate Eq _ _ = Right FalseVal

operate Less (IntVal x) (IntVal y) = Right (if x < y then TrueVal else FalseVal)
operate Greater (IntVal x) (IntVal y) = Right (if x > y then TrueVal else FalseVal)

operate In v (ListVal mList) = if foldl (\acc x -> (operate Eq x v == Right TrueVal) || acc) False mList
  then
    Right TrueVal
  else
    Right FalseVal

apply :: FName -> [Value] -> Comp Value
apply "range" [IntVal v1, IntVal v2, IntVal 0] = abort (EBadArg "the step in range can not be 0")
apply "range" [IntVal v1, IntVal v2, IntVal v3] =  return $ ListVal (map IntVal l)
  where l = [x | x <- [v1..v2], x /= v2, (x - v1) `mod` v3 == 0]

apply "range" [IntVal v1, IntVal v2] = apply "range" [IntVal v1, IntVal v2, IntVal 1]
apply "range" [IntVal v1] = apply "range" [IntVal 0, IntVal v1, IntVal 1]
apply "range" _ = abort (EBadArg "the parameters of range is wrong")

-- newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }
-- todo print具有多个参数时，中间需要用空格隔开
-- todo Comp [String]中，每个element是一行么？
apply "print" [] = return NoneVal
apply "print" (x:xs) = do
  v1 <- printRes x
  v2 <- apply "print" xs
  return NoneVal
  where
    printRes :: Value -> Comp Value
    printRes val = Comp{runComp = const (Right NoneVal, [value2String val])}
    -- printResWithSpace :: Value -> Comp Value
    -- printResWithSpace val = Comp{runComp = const (Right NoneVal, [value2String val ++ " "])}


value2String :: Value -> String
value2String NoneVal = "None"
value2String TrueVal = "True"
value2String FalseVal = "False"
value2String (IntVal x) = show x
value2String (StringVal s) = s       -- Strings are printed directly, without any outer quotes
value2String (ListVal l) = "[" ++
  fst
    (foldl
      (\acc x -> (if snd acc + 1 == length l then fst acc ++ x else fst acc ++ x ++ ", " , snd acc + 1) )
      ("", 0)
      (map value2String l) )
  ++ "]"


-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = do return v
eval (Var v) = look v
eval (Oper Plus e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal x, IntVal y) -> return $ IntVal (x + y)
    _ -> abort (EBadArg "Can only plus Int")

eval (Oper Minus e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal x, IntVal y) -> return $ IntVal (x - y)
    _ -> abort (EBadArg "Can only minus Int")

eval (Oper Times e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal x, IntVal y) -> return $ IntVal (x * y)
    _ -> abort (EBadArg "Can only times Int")

eval (Oper Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal x, IntVal y) -> return $ IntVal (x `div` y)
    _ -> abort (EBadArg "Can only div Int")

eval (Oper Mod e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal x, IntVal y) -> return $ IntVal (x `mod` y)
    _ -> abort (EBadArg "Can only mod Int")

--  todo 不同类型之间无法比较
eval (Oper Eq e1 e2) = do {v1 <- eval e1;
      v2 <- eval e2;
      case (v1, v2) of
        (NoneVal, NoneVal) -> return TrueVal
        (IntVal x, IntVal y) -> return $ if x ==y then TrueVal else FalseVal
        (TrueVal, FalseVal) -> return FalseVal
        (TrueVal, TrueVal) -> return TrueVal
        (FalseVal, FalseVal) -> return TrueVal
        (FalseVal, TrueVal) -> return FalseVal
        (StringVal s1, StringVal s2) -> return $ if s1 == s2 then TrueVal else FalseVal
        (ListVal s1, ListVal s2) -> return $ if s1 == s2 then TrueVal else FalseVal
        _ ->  abort (EBadArg "Can only eq same types")
    }

eval (Oper Less e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal x, IntVal y) -> return $ if x < y then TrueVal else FalseVal
    _ -> abort (EBadArg "Can only compare Int")

eval (Oper Greater e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal x, IntVal y) -> return $ if x > y then TrueVal else FalseVal
    _ -> abort (EBadArg "Can only compare Int")

eval (Oper In e1 mList) = do
  v1 <- eval e1
  v2 <- eval mList
  case operate In v1 v2 of
    Left e -> abort (EBadArg "In EBadArg")
    Right a -> return a

eval (Not exp) = do
  v1 <- eval exp
  case v1 of
    NoneVal -> return TrueVal
    TrueVal -> return FalseVal
    FalseVal -> return TrueVal
    IntVal 0 -> return TrueVal
    IntVal x -> return FalseVal
    StringVal "" -> return TrueVal
    StringVal s -> return FalseVal
    ListVal [] -> return TrueVal
    ListVal s -> return FalseVal

eval (List []) = do
    return $ ListVal []

eval (List (x:xs)) = do
    xValue <- eval  x
    xsValue <- eval $ List xs
    case xsValue of
      ListVal l -> return $ ListVal (xValue:l)

--  todo 为啥ListVal mList <- acc 这样的模式匹配不行？
eval (Call fn expList) = do
    listVal <- foldl (\acc x -> do
                  value <- eval x
                  accValue <- acc
                  case accValue of
                    ListVal accListVal -> return $ ListVal(accListVal ++ [value]))
                  (return $ ListVal []) expList
    case listVal of
      ListVal accListVal -> apply fn accListVal


exec :: Program -> Comp ()
exec [] = Comp{runComp = const (Right (), [""])}
exec ((SDef vname exp):stmts) = do
  eval exp >>= (\v -> withBinding vname v (exec []))
  exec stmts
exec ((SExp exp):stmts) = do 
  eval exp
  exec stmts

-- newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

execute :: Program -> ([String], Maybe RunError)
execute [] = ([], Nothing)
execute stmts = (snd result, case fst result of 
    Left runerr -> Just runerr
    Right _ -> Nothing
  )
  where result = runComp (exec stmts) []
