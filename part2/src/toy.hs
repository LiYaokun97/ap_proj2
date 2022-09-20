
import Control.Monad

type Env = [(String, Int)]

newtype Comp a = Comp {runComp :: Env -> (a, [(String, Int)]) }

instance Monad Comp where
  return x = Comp (const (x, []) )
  m >>= f = Comp {runComp =
     \env -> let (a, _) = runComp m env in runComp (f a) env
     }


instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

bindData :: String -> Int -> Comp a -> Comp a
bindData name v comp0 = Comp (\env -> runComp comp0 ((name, v):env))


saveEnv :: Comp a-> Comp a
saveEnv comp0 = Comp (\env -> let (a, originalList) = runComp comp0 env in (a, originalList ++ env))

data Exp = Var String


getBinding :: Env -> String -> Int
getBinding env v = foldr (\x acc -> if fst x == v then snd x else acc) (-1000000) env


look :: String -> Comp Int
look v = Comp {runComp = \env -> (getBinding env v, env)}


eval :: Exp -> Comp Int
eval (Var s) = look s


test0 = bindData "x" 100 (return ()) >> eval (Var "x")

test1 = saveEnv test0
main :: IO()
main = let (x, list) = runComp test1 [] in do
    print(x, list)


