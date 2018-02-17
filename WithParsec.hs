import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Data.Char (toUpper)
import LispAST

lispIden = alphaNum <|> oneOf "!$%&*+-./:<=>?@^_~" --TODO: https://stackoverflow.com/a/11684133

lispLanguage = emptyDef { 
    identStart = lispIden,
    identLetter = lispIden,
    caseSensitive = False
}

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , integer = m_integer
           , whiteSpace = m_whiteSpace } = makeTokenParser lispLanguage

atomParser :: Parser Atom
atomParser = fmap (Integer . fromInteger) (try m_integer) <|> fmap identifierToAtom m_identifier

identifierToAtom x = case map toUpper x of
                          "T" -> T
                          "NIL" -> Nil
                          _ -> Symbolic x

sexpParser :: Parser SExpression
sexpParser = (m_parens (sexp_inner <|> return (Atomic Nil)) <|> fmap Atomic atomParser)
    where 
        sexp_inner = sexpParser >>= \left -> do { dot; right <- sexpParser; return (Tree left right) } 
          <|> (fmap (Tree left . (foldr Tree (Atomic Nil))) $ many sexpParser)

mainParser = m_whitespace >> many sexpParser
