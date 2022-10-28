-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing),

    testCase "type integer" $
    execute [SExp (Oper Plus (Const (IntVal 3)) (Const (StringVal "a")))]
    @?= ([], Just (EBadArg "Can only plus Int")),

    testCase "type list" $
    execute [SExp (Oper In (Const (StringVal "1")) (Const (StringVal "123")))]
    @?= ([], Just (EBadArg "In EBadArg")),

    testCase "type range" $
    execute [SDef "squares"(Compr (Oper Times (Var "x") (Var "x"))[CCFor "x" (Call "range" [Const (IntVal 1), Const (IntVal 10), Const (IntVal 0)])])]
    @?= ([], Just (EBadArg "the step in range can not be 0")),

    testCase "type range 2" $
    execute [SDef "squares"(Compr (Oper Times (Var "x") (Var "x"))[CCFor "x" (Call "range" [ Const (StringVal "123")])])]
    @?= ([], Just (EBadArg "the parameters of range is wrong")),     
        

    testCase "oper test" $
    execute [     
      SDef "x" (Const (IntVal 100)),
      SDef "xs" (Const (ListVal [(IntVal 1000), (IntVal 100)])),
      SDef "y" (Const (ListVal [(IntVal 1000), TrueVal, NoneVal])),
      SDef "z" (Const (ListVal [(IntVal 1000), ListVal [NoneVal, (IntVal 200)], NoneVal])),
      SDef "empty" (Const (ListVal [])),
      SExp (Call "print" [Oper In (Var "x") (Var "xs")]),
      SExp (Call "print" [Oper In (Var "x") (Var "y")]),
      SExp (Call "print" [Oper In (Var "x") (Var "z")]),
      SExp (Call "print" [Oper In (Var "x") (Var "empty")])]
    @?= (["True","False","False","False"], Nothing)]
