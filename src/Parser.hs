module Main (main) where

import Lexer
import Tokens
import Text.Parsec
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe

type MemoryCell = (Token,Token)
type Memory = [MemoryCell]

-- funções auxiliares

extractId :: Token -> String
extractId (ID x p) = x

-- parsers para os não-terminais

program :: ParsecT [Token] Memory IO([Token])
program = do
            a <- stmts
            eof
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
            a <- printStm <|> printlnStm
            return a

printStm :: ParsecT [Token] Memory IO([Token])
printStm = do
            a <- (printToken <?> "print")
            b <- (openRoundToken <?> "(")
            c <- expression
            d <- (closeRoundToken <?> ")")
            e <- (semiColonToken <?> ";")
            liftIO (print c)
            return [c]

printlnStm :: ParsecT [Token] Memory IO([Token])
printlnStm = do
            a <- (printlnToken <?> "print")
            b <- (openRoundToken <?> "(")
            c <- expression
            d <- (closeRoundToken <?> ")")
            e <- (semiColonToken <?> ";")
            liftIO (print c)
            return [c]

expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = do
                a <- exprN1
                b <- (plusToken <|> minusToken)
                c <- exprN1
                return (eval a b c)

exprN1 :: ParsecT [Token] [(Token,Token)] IO(Token)
exprN1 = do
                a <- exprN2
                b <- (multiplyToken <|> divideToken)
                c <- exprN2
                return (eval a b c)

exprN2 :: ParsecT [Token] [(Token,Token)] IO(Token)
exprN2 = do
            a <-(valueFloatToken <|> valueIntToken)
            return a

-- funções para o avaliador de expressões

eval :: Token -> Token -> Token -> Token
eval (ValueInt x p) (Plus _) (ValueInt y _) = ValueInt (x + y) p
eval (ValueInt x p) (Minus _) (ValueInt y _) = ValueInt (x - y) p
eval (ValueInt x p) (Multiply _) (ValueInt y _) = ValueInt (x * y) p

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
                Right ans -> print ans
            }