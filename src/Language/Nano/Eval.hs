{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
    ( Env,
      Value(VInt, VErr, VNil, VClos, VBool, VPrim, VPair),
      Expr(..),
      Id,
      Binop(..),
      Error(Error) )
import Language.Nano.Parser

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
-- 0
--
-- >>> eval env0 "p"
-- Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- Error {errMsg = "type error: evalOp"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- Error {errMsg = "type error: evalOp"}
--
-- >>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus "x" "y"
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- unknown command '{'
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- Error {errMsg = "type error: evalOp"}
-- >>> execExpr (EApp "head" el)
-- Error {errMsg = "unbound variable: head"}
-- >>> execExpr (EApp "tail" el)
-- Error {errMsg = "unbound variable: tail"}
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval _ (EInt x) = VInt x
eval _ (EBool b) = VBool b
eval _ ENil = VNil

eval env (EVar v) = lookupId v env

eval env (EBin op e1 e2) = evalOp op (eval env e1) (eval env e2)

eval env (EIf e1 e2 e3) | eval env e1 == VBool True = eval env e2
                        | eval env e1 == VBool False = eval env e3
                        | otherwise = throw (Error "type error: eval IF")

eval env (ELet x e1 e2) = eval env' e2
                        where
                          env' = (x, eval env' e1) : env


eval env (EApp e1 e2) = case eval env e1 of
                       VClos closureEnv closureFormal closureBody -> eval env' closureBody
                                                        where
                                                          env' = (closureFormal, eval env e2) : closureEnv
                       VPrim function -> function (eval env e2)
                       unknownApp -> throw (Error ("type error EApp" ++ show unknownApp))



eval env (ELam formal body) = VClos env formal body



--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus  (VInt a) (VInt b) = VInt (a + b)
evalOp Minus (VInt a) (VInt b) = VInt (a - b)
evalOp Mul   (VInt a) (VInt b) = VInt (a * b)
evalOp Div   (VInt a) (VInt b) = VInt (a `div` b)

evalOp Eq   (VPair x y) (VPair a b) | getHead(VPair x y) == getHead(VPair a b) = evalOp Eq (getTail(VPair x y)) (getTail(VPair a b))
                                    | otherwise = VBool False
evalOp Eq    (VInt a)  (VInt b) = VBool (a == b)

evalOp Eq    (VBool a) (VBool b) = VBool (a == b)
evalOp Eq    VNil VNil = VBool True
evalOp Eq     _     VNil = VBool False
evalOp Eq     VNil     _ = VBool False

evalOp Ne (VPair x y) (VPair a b) | getHead(VPair x y) == getHead(VPair a b) = evalOp Ne (getTail(VPair x y)) (getTail(VPair a b))
                                  | otherwise = VBool True
evalOp Ne    (VInt a)  (VInt b) = VBool (a /= b)
evalOp Ne    (VBool a) (VBool b) = VBool (a /= b)

evalOp Ne    VNil VNil = VBool False
evalOp Ne      _    VNil = VBool True
evalOp Ne    VNil   _ = VBool True

evalOp Le    (VInt a) (VInt b) = VBool (a <= b)
evalOp Lt    (VInt a) (VInt b) = VBool (a < b)

evalOp And   (VBool a) (VBool b) = VBool (a&&b)
evalOp Or    (VBool a) (VBool b) = VBool (a||b)

evalOp Cons (VPair x VNil) (VPair a b) = VPair x (VPair a b)
evalOp Cons (VPair x y) (VPair a b) = VPair x (evalOp Cons y (VPair a b) )
evalOp Cons a b = VPair a b

evalOp _ _ _ = throw (Error "type error: evalOp")


--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId y ((z,v):env) | y == z = v
                       | otherwise = lookupId y env
lookupId y _ = throw (Error ("unbound variable: " ++ y))


getHead :: Value -> Value
getHead (VPair a _) = a
getHead _ = throw (Error "type error: getHead")

getTail :: Value -> Value
getTail (VPair _ b) = b
getTail _ = throw (Error "type error: getTail")

prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
    ("head", VPrim getHead),
    ("tail", VPrim getTail)
  ]

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------
