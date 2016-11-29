{- Main.hs  -}
import           Control.Applicative ( (*>), (<$>), (<*) )
import           Data.Char           ( ord )
import           Text.Parsec         ( (<|>), char, oneOf, parseTest )
import           Text.Parsec.String  ( Parser )

data Exp = Add Exp Exp
         | Mul Exp Exp
         | Nat Int
    deriving Show

expr = Add <$> term <$> (char '+' *> expr) <|> term

term = Mul <$> factor <$> (char '*' *> term) <|> factor

factor = (char '(' *> expr <* char ')') <|> nat

nat = Nat . charToInt <$> oneOf ['0' .. '9']
  where
    charToInt c = ord c - ord '0'

main = do
    parseTest expr "1+2*3"
    parseTest expr "(1+2)*3"
