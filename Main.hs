import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad (MonadPlus, mzero, mplus, guard, liftM2)
import Prelude hiding (fail)
import Parser
import Data.Char (isDigit, isSpace)

type Identifier = String

data SExpression = Atomic Atom | Tree SExpression SExpression
data Atom = Integer Int | Symbolic Identifier | T | Nil deriving Show --TODO: Doubles?

instance Show SExpression where
    show (Atomic a) = show a
    show (Tree l r) = '(' : show l ++ " . " ++ show r ++ ")"

car ((Tree l r):[]) = l
cdr ((Tree l r):[]) = r
cons :: [SExpression] -> SExpression
cons (l:r:[]) = Tree l r

--functions :: Map (Identifier, Int) ([SExpression] -> SExpression)
--functions = Map.fromList [(("car", 1), car), (("cdr", 1), cdr), (("cons", 2), cons)]

--Tokenization
data Token = OpenParen | CloseParen | Period | Number Int | Identifier Identifier deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':x) = OpenParen : tokenize x
tokenize (')':x) = CloseParen : tokenize x
tokenize (xs@('.':x)) = case x of 
                        [] -> [Period]
                        c:x' -> if isTerminating c then Period : tokenize x else tokenizeIden xs
tokenize (xs@(hd:prog)) = if isSpace hd 
                          then tokenize (dropWhile isSpace prog)
                          else tokenizeIden xs
                          
tokenizeIden xs = let iden = (takeWhile (not . isTerminating) xs) in
                      if all isDigit iden 
                         then Number (read iden) : tokenize (dropWhile (not . isTerminating) xs) --TODO: splitWhile?
                         else Identifier iden : tokenize (dropWhile (not . isTerminating) xs) --TODO: splitWhile?

isTerminating = liftM2 (||) isSpace (liftM2 (||) (==')') (=='('))



parseSExp :: Parser [Token] SExpression
parseSExp = parseTree <|> parseList <|> parseAtom

parseTree = do
    parseElem OpenParen
    left <- parseSExp
    parseElem Period
    right <- parseSExp
    parseElem CloseParen
    return (Tree left right)
    
parseList = do
    parseElem OpenParen
    elems <- kleeneStar parseSExp
    parseElem CloseParen
    return (foldr (\e acc -> Tree e acc) (Atomic Nil) elems)
    
parseAtom = parseInt <|> parseConstOrIdentifier

parseInt = do
    tokenHead <- parseAnyElem
    case tokenHead of
         Number x -> return (Atomic (Integer x))
         x -> mzero

parseConstOrIdentifier = do
    tokenHead <- parseAnyElem
    case tokenHead of
         Identifier i -> if i == "NIL" then return (Atomic Nil) else if i == "T" then return (Atomic T) else return (Atomic (Symbolic i))
         x -> mzero
