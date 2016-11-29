module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Either (Either(..))
import Test.Assert (ASSERT, assert')
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)
import Prelude hiding (between, when)


parseTest :: forall s a eff. (Show a, Eq a) => s -> a -> Parser s a -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseTest input expected p =
  case runParser input p of
    Right actual -> do
      assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
      logShow actual
    Left err ->
      assert' ("error: " <> show err) false


main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  parseTest "abcd" "abcd" $ string "abcd"
