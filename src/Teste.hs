module Teste (main) where

    import Lexer
    import Text.Parsec
    import Control.Monad.IO.Class
    
    import System.IO

process :: IO ()
process = do
    s <- readFile "exemplo.kod"
    print s