module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe

type MemoryCell = (Token,Token)
type Memory = [MemoryCell]

-- funções auxiliares

extractId :: Token -> String
extractId (ID x p) = x

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

openRoundToken = tokenPrim show update_pos get_token where
    get_token (OpenRound p) = Just (OpenRound p)
    get_token _           = Nothing

closeRoundToken = tokenPrim show update_pos get_token where
    get_token (CloseRound p) = Just (CloseRound p)
    get_token _           = Nothing

printToken = tokenPrim show update_pos get_token where
    get_token (Print p) = Just (Print p)
    get_token _           = Nothing

printlnToken = tokenPrim show update_pos get_token where
    get_token (Println p) = Just (Println p)
    get_token _           = Nothing

valueFloatToken :: ParsecT [Token] st IO (Token)
valueFloatToken = tokenPrim show update_pos get_token where
    get_token (ValueFloat x p) = Just (ValueFloat x p)
    get_token _           = Nothing

valueIntToken :: ParsecT [Token] st IO (Token)
valueIntToken = tokenPrim show update_pos get_token where
    get_token (ValueInt x p) = Just (ValueInt x p)
    get_token _           = Nothing

valueBoolToken :: ParsecT [Token] st IO (Token)
valueBoolToken = tokenPrim show update_pos get_token where
    get_token (ValueBool x p) = Just (ValueBool x p)
    get_token _           = Nothing

valueStringToken :: ParsecT [Token] st IO (Token)
valueStringToken = tokenPrim show update_pos get_token where
    get_token (ValueString x p) = Just (ValueString x p)
    get_token _           = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  

-- parsers para os não-terminais

program :: ParsecT [Token] Memory IO([Token])
program = do
            a <- stmts
            s <- getState
            eof
            liftIO (print s)
            return (a)

stmts :: ParsecT [Token] Memory IO([Token])
stmts = do
          a <- many (varDeclaration <|> assign <|> ioStm)
          return (concat a)

varDeclaration :: ParsecT [Token] Memory IO([Token])
varDeclaration = do
            a <- (letToken <?> "let")
            b <- (idToken <?> "identifier")
            c <- assignmentToken
            d <- (valueFloatToken <|> valueIntToken)
            e <- (semiColonToken <?> ";")
            updateState(symtableInsert (b, d))
            return [b,d]

assign :: ParsecT [Token] Memory IO([Token])
assign = do
            a <- idToken
            b <- assignmentToken
            c <- (valueFloatToken <|> valueIntToken)
            d <- semiColonToken
            updateState(symtableUpdate (a, c))
            return [a,c]

ioStm :: ParsecT [Token] Memory IO([Token])
ioStm = do 
            a <- printStm <|> assign
            return a

printStm :: ParsecT [Token] Memory IO([Token])
printStm = do
            a <- printToken
            b <- openRoundToken
            c <- idToken
            d <- closeRoundToken
            e <- semiColonToken
            return [c]

printlnStm :: ParsecT [Token] Memory IO([Token])
printlnStm = do
            a <- printlnToken
            b <- openRoundToken
            c <- idToken
            d <- closeRoundToken
            e <- semiColonToken
            return [c]

-- funções para verificação de tipos

getDefaultValue :: Token -> Token
getDefaultValue (TypeInt8 (l, c)) = ValueInt 0 (l, c)

getType :: Token -> Memory -> Token
getType _ [] = error "Variable not found"
getType (ID id1 p1) ((ID id2 _, value):t) = if id1 == id2 then value
                                             else getType (ID id1 p1) t

compatible :: Token -> Token -> Bool
compatible (ValueInt _ _) (ValueInt _ _) = True
compatible _ _ = False

-- funções para a tabela de símbolos

symtableInsert :: MemoryCell -> Memory -> Memory
symtableInsert symbol []  = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]

symtableUpdate :: MemoryCell -> Memory -> Memory
symtableUpdate _ [] = fail "variable not found"
symtableUpdate (ID id1 p1, v1) ((ID id2 p2, v2):t) = 
                               if id1 == id2 then (ID id1 p2, v1) : t
                               else (ID id2 p2, v2) : symtableUpdate (ID id1 p1, v1) t

symtableRemove :: MemoryCell -> Memory -> Memory
symtableRemove _ [] = fail "variable not found"
symtableRemove (id1, v1) ((id2, v2):t) = 
                                if id1 == id2 then t
                                else (id2, v2) : symtableRemove (id1, v1) t

-- invocação do parser para o símbolo de partida

printTokens = do contents <- readFile "exemplo.kod"
                 print (scanTokens contents)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Failed" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "exemplo.kod")) of
            { Left err -> print err;
                Right ans -> print "Finished"
            }