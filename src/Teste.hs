module Teste (main) where

    import Lexer
    import Text.Parsec
    import Control.Monad.IO.Class
    
    import System.IO.Unsafe

main :: IO ()
main = print (getTokens "exemplo.kod")