import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad (MonadPlus, mzero, mplus, guard, liftM2)
import Prelude hiding (fail)
import Parser
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

type Identifier = String

data SExpression = Atomic Atom | Tree SExpression SExpression
data Atom = Integer Int | Symbolic Identifier | T | Nil --TODO: Doubles?
data Constant = CInteger Int | CT | CNil

instance Show Constant where
    show (CInteger a) = show a
    show CT = "T"
    show CNil = "NIL"

type AList = SExpression
type DList = SExpression

instance Show SExpression where
    show (Atomic a) = show a
    show (Tree l r) = '(' : show l ++ " . " ++ show r ++ ")"

instance Show Atom where
    show (Integer i) = show i
    show (Symbolic s) = s
    show T = "T"
    show Nil = "Nil"
    
car (Tree (Tree l r) (Atomic Nil)) = l
cdr (Tree (Tree l r) (Atomic Nil)) = r
cons (Tree s1 (Tree s2 (Atomic Nil))) = Tree s1 s2
add (Atomic Nil) = 0
add (Tree (Atomic (Integer i)) r) = i + add r

interpret :: String -> IO ()
interpret = print . snd . evaluate (Atomic Nil) (Atomic Nil) . snd . fromJust . tryParse parseSExp . tokenize

constantToAtom :: Constant -> SExpression
constantToAtom (CInteger a) = Atomic (Integer a)
constantToAtom CT = Atomic T
constantToAtom CNil = Atomic Nil

atomToConstant :: SExpression -> Constant
atomToConstant sexpr = let (Atomic a) = sexpr in case a of
                                Integer i -> CInteger i
                                T -> CT
                                Nil -> CNil 

evaluate :: AList -> DList -> SExpression -> (DList, SExpression) --TODO: Clean up stateful computation (maybe with Control.Monad.State)
evaluate aList dList sexpr = case sexpr of
                            Atomic a -> (dList, Atomic a)
                            Tree l r -> evaluateFunction aList dList l r
                            
searchAList :: AList -> Identifier -> SExpression
searchAList (Atomic Nil) t = error ("Unbound token " ++ t)
searchAList (Tree l r) t = let (Tree (Atomic (Symbolic name)) value) = l in
                               if name == t then value else searchAList r t

searchDList :: DList -> Identifier -> SExpression
searchDList = searchAList
                        
bindParam :: AList -> DList -> SExpression -> SExpression -> AList
bindParam _ _ (Atomic Nil) (Atomic Nil) = (Atomic Nil)
bindParam aList dList (Tree (Atomic (Symbolic formalName)) rest) (Tree val otherParams) = Tree (Tree (Atomic $ Symbolic formalName) val) (bindParam aList dList rest otherParams)
bindParam _ _ _ _ = error "Incorrect number of parameters."
                               
concatAList :: AList -> AList -> SExpression
concatAList (Atomic Nil) x = x
concatAList (Tree l r) x = Tree l (concatAList r x)

evalParams aList dList (Atomic Nil) = Atomic Nil
evalParams aList dList (Tree l r) = Tree (snd $ evaluate aList dList l) (evalParams aList dList r)

evaluateFunction :: AList -> DList -> SExpression -> SExpression -> (DList, SExpression)
evaluateFunction aList dList func params = case func of
                                          (Atomic (Symbolic "defun")) -> undefined
                                          (Atomic (Symbolic "quote")) -> case params of (Tree l (Atomic Nil)) -> (dList, l)
                                          (Atomic (Symbolic "lambda")) -> (dList, undefined) --Use undefined to return a void semantic value for SExpression. It shouldn't be read, in general.
                                          (Tree _ _) -> evaluateFunction aList dList (snd $ evaluate aList dList func) params
                                          (Atomic (Symbolic s)) -> let evaledParams = evalParams aList dList params in case s of 
                                                                            "car" -> (dList, car evaledParams)
                                                                            "cdr" -> (dList, cdr evaledParams)
                                                                            "cons" -> (dList, cons evaledParams)
                                                                            "+" -> (dList, add evaledParams)
                                                                            _ -> let (Tree paramList (Tree body (Atomic Nil))) = searchDList dList s in
                                                                                evaluate (concatAList (bindParam aList dList paramList evaledParams) aList) dList body
                                          x -> error (show x)
                               
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
