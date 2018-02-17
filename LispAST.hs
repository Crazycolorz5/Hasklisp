module LispAST (Identifier, SExpression (..), Atom (..)) where

type Identifier = String

data SExpression = Atomic Atom | Tree SExpression SExpression
data Atom = Integer Int | Symbolic Identifier | T | Nil --TODO: Doubles?

instance Show SExpression where
    show (Atomic a) = show a
    show (Tree l r) = '(' : show l ++ " . " ++ show r ++ ")"

instance Show Atom where
    show (Integer i) = show i
    show (Symbolic s) = s
    show T = "T"
    show Nil = "Nil"
