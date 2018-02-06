import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type Identifier = String

data SExpression = Atomic Atom | Tree SExpression SExpression
data Atom = Integer Int | Real Double | Character Char | Identifier Identifier | Nil deriving Show

instance Show SExpression where
    show (Atomic a) = show a
    show (Tree l r) = '(' : show l ++ " . " ++ show r ++ ")"

car ((Tree l r):[]) = l
cdr ((Tree l r):[]) = r
cons :: [SExpression] -> SExpression
cons (l:r:[]) = Tree l r

functions :: Map (Identifier, Int) ([SExpression] -> SExpression)
functions = Map.fromList [(("car", 1), car), (("cdr", 1), cdr), (("cons", 2), cons)]
