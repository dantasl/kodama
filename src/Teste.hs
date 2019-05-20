module Teste where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import System.IO

galado :: IO ()
galado = do contents <- readFile "exemplo.kod" -- use '<-' here so that 'contents' is a String
            print $ getTokens contents -- split on newlines