module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe

-- parsers para os tokens

letToken = tokenPrim show update_pos get_token where
    get_token (Let p) = Just (Let p)
    get_token _           = Nothing

idToken = tokenPrim show update_pos get_token where
    get_token (ID x p) = Just (ID x p)
    get_token _        = Nothing

assignmentToken = tokenPrim show update_pos get_token where
    get_token (Assignment p) = Just (Assignment p)
    get_token _           = Nothing

semiColonToken = tokenPrim show update_pos get_token where
    get_token (SemiColon p) = Just (SemiColon p)
    get_token _           = Nothing

valueFloatToken = tokenPrim show update_pos get_token where
    get_token (ValueFloat x p) = Just (ValueFloat x p)
    get_token _           = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- stmts
            eof
            return (a)

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remaining_stmts = (do a <- assign
                      return (a)) <|> (return [])

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
            a <- letToken
            b <- idToken
            c <- assignmentToken
            d <- valueFloatToken
            e <- semiColonToken
            return ([b,d])

-- funções para a tabela de símbolos

-- invocação do parser para o símbolo de partida

printTokens = do contents <- readFile "exemplo.kod"
                 print (scanTokens contents)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "exemplo.kod")) of
            { Left err -> print err;
                Right ans -> print ans
            }