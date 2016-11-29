{- Main.hs  -}
import           Text.Parsec        ( parseTest, string )
import           Text.Parsec.String ( Parser )

main = do
    parseTest (string "abcd") "abcd"
