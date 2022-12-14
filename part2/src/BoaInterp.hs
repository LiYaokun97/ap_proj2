-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use const" #-}

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a  = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return x = Comp (\_ -> (Right x, []))
  (>>=) :: Comp a -> (a -> Comp b) -> Comp b
  m >>= f = Comp {runComp = \env ->
    case runComp m env of
        (Left e, s) -> (Left e, s)
        (Right a, s) -> case runComp (f a) env of
                (Left e, s') -> (Left e, s ++ s')
                (Right a, s')-> (Right a, s ++ s')
      }


-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort runError = Comp {runComp = const (Left runError, [])}

getBinding :: Env -> VName -> Value
getBinding env v = foldr (\x acc -> if fst x == v then snd x else acc) NoneVal env

isVarInEnv :: Env -> VName -> Bool
isVarInEnv env v = foldr (\x acc -> (fst x == v) || (acc || False)) False env

look :: VName -> Comp Value
look v = Comp {runComp = \env -> if isVarInEnv env v
  then
    (Right (getBinding env v), [])
  else
    (Left (EBadVar v), [] )
  }

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding v value comp0 = Comp (\env -> runComp comp0 ((v, value):env))

output :: String -> Comp ()
output s = Comp{runComp = \_ -> (Right (), [s])}

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal  = False
truthy TrueVal = True
truthy FalseVal = False
truthy (IntVal 0) = False
truthy (IntVal _) = True
truthy (StringVal "") = False
truthy (StringVal _) = True
truthy (ListVal []) = False
truthy (ListVal _) = True

operate :: Op -> Value -> Value -> Either String Value

operate Plus (IntVal x) (IntVal y) = Right (IntVal (x + y))
operate Plus _ _ = Left  "plus can only accept integers"

operate Minus (IntVal x) (IntVal y) = Right (IntVal (x - y))
operate Minus _ _ = Left  "minus can only accpet integers"

operate Times (IntVal x) (IntVal y) = Right (IntVal (x * y))
operate Times _ _ = Left  "times can only accept integers"

operate Div (IntVal _) (IntVal 0) = Left  "divide by zero"
operate Div (IntVal x) (IntVal y) = Right (IntVal (x `div` y))
operate Div _ _ = Left  "div can only accept integers"

operate Mod (IntVal _) (IntVal 0) = Left "mod by zero"
operate Mod (IntVal x) (IntVal y) = Right (IntVal (x `mod` y))
operate Mod _ _ = Left  "mod can only accept integers"

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
operate Less _ _ = Left "can only compare numbers"

operate Greater (IntVal x) (IntVal y) = Right (if x > y then TrueVal else FalseVal)
operate Greater _ _ = Left "can only compare numbers"

-- we have to break from the loop if we find v, so can not use fold to implement In
operate In v (ListVal mList) = if helper v mList
  then
    Right TrueVal
  else
    Right FalseVal
  where helper _ [] = False
        helper v' (x:xs) = (operate Eq v' x == Right TrueVal) || helper v' xs

operate In _ _ = Left "in can only used in list variable"


apply :: FName -> [Value] -> Comp Value
apply "range" [IntVal _, IntVal _, IntVal 0] = abort (EBadArg "the step in range can not be 0")

apply "range" [IntVal v1, IntVal v2, IntVal v3] | v1 >= v2 && v3 > 0 = return $ ListVal []
apply "range" [IntVal v1, IntVal v2, IntVal v3] | v1 <= v2 && v3 < 0 = return $ ListVal []
apply "range" [IntVal v1, IntVal v2, IntVal v3] | v3 > 0 =  return $ ListVal (map IntVal (helper v1 v2 v3 0))
      where
        helper :: Int -> Int -> Int -> Int -> [Int]
        helper start end step n = if (start + n* step) >= end then [] else (start + n * step) : helper start end step (n+1)

apply "range" [IntVal v1, IntVal v2, IntVal v3] | v3 < 0 =  return $ ListVal (map IntVal (helper v1 v2 v3 0))
      where
        helper :: Int -> Int -> Int -> Int -> [Int]
        helper start end step n = if (start + n* step) <= end then [] else (start + n * step) : helper start end step (n+1)


apply "range" [IntVal v1, IntVal v2] = apply "range" [IntVal v1, IntVal v2, IntVal 1]
apply "range" [IntVal v1] = apply "range" [IntVal 0, IntVal v1, IntVal 1]
apply "range" _ = abort (EBadArg "the parameters of range is wrong")

apply "print" [] = Comp {runComp = \_ -> (Right NoneVal, [""])}
apply "print" argList = Comp{runComp = \_ -> (Right NoneVal, [getPrintResult argList])}

apply f _ = abort $ EBadFun f

getPrintResult :: [Value] -> String
getPrintResult argList = fst (foldl
  (\acc x ->
    (if (snd acc + 1) == length argList
      then fst acc ++ value2String x
      else fst acc ++ value2String x ++ " ",
      snd acc + 1
    )
  )
  ("" , 0)
  argList)


value2String :: Value -> String
value2String NoneVal = "None"
value2String TrueVal = "True"
value2String FalseVal = "False"
value2String (IntVal x) = show x
value2String (StringVal s) = s      
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
    (IntVal _, IntVal 0) -> abort (EBadArg "divide by zero")
    (IntVal x, IntVal y) -> return $ IntVal (x `div` y)
    _ -> abort (EBadArg "Can only div Int")

eval (Oper Mod e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal _, IntVal 0) -> abort (EBadArg "mod by zero")
    (IntVal x, IntVal y) -> return $ IntVal (x `mod` y)
    _ -> abort (EBadArg "Can only mod Int")

eval (Oper Eq e1 e2) = do {
      v1 <- eval e1;
      v2 <- eval e2;
      return $ if v1 == v2 then TrueVal else FalseVal
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
    Left _ -> abort (EBadArg "In EBadArg")
    Right a -> return a

eval (Not exp) = do
  v1 <- eval exp
  return $ if truthy v1 then FalseVal else TrueVal

eval (List []) = do
    return $ ListVal []

eval (List (x:xs)) = do
    xValue <- eval  x
    xsValue <- eval $ List xs
    case xsValue of
      ListVal l -> return $ ListVal (xValue:l)

eval (Call fn expList) = do
    listVal <- foldl (\acc x -> do
                  value <- eval x
                  accValue <- acc
                  case accValue of
                    ListVal accListVal -> return $ ListVal(accListVal ++ [value]))
                  (return $ ListVal []) expList
    case listVal of
      ListVal argList -> apply fn argList

{-
  eval Compr
  to do list comprehension, list-valued expressions need to be rewrite
  through concatMap. The link above provide formula to do that, which is really helpful.
  https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11
-}
eval (Compr exp clauseList) = do
    valueList <- clauseHelper exp clauseList
    return (ListVal valueList)

clauseHelper :: Exp -> [CClause] -> Comp [Value]
clauseHelper exp clauseList = case clauseList of
  [] -> sequence [eval exp]
  x:xs -> case x of
    CCIf expIf -> do
      value <- eval expIf
      (if truthy value
        then clauseHelper exp xs
        else return [])

    CCFor vname expFor -> do
      l <- eval expFor
      case l of
        {- 
          actually the following expression is doing ConcatMap, because we have to return 
          type Comp Value, we need to use sequence to do some special operations to get it.
        -}
        ListVal valueList -> do
          result <- sequence [withBinding vname value (clauseHelper exp xs)| value <- valueList]
          return (concat result)
        _ -> sequence [abort (EBadArg "the expression for CCFor must be list")]
    

exec :: Program -> Comp ()
exec [] = do return ()
exec ((SDef vname exp):stmts) = do
      value <- eval exp
      withBinding vname value (exec stmts)

exec ((SExp exp):stmts) = do
  eval exp
  exec stmts

execute :: Program -> ([String], Maybe RunError)
execute [] = ([], Nothing)
execute stmts = let result = runComp (exec stmts) [] in
  (snd result, case fst result of
    Left runerr -> Just runerr
    Right _ -> Nothing
  )
