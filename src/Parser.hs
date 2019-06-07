module Main (main) where

import Lexer
import Tokens
import Values
import Memory
import Text.Parsec
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe

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
            liftIO (putStr (show (extractValue c)))
            return [c]

printlnStm :: ParsecT [Token] Memory IO([Token])
printlnStm = do
            a <- (printlnToken <?> "print")
            b <- (openRoundToken <?> "(")
            c <- expression
            d <- (closeRoundToken <?> ")")
            e <- (semiColonToken <?> ";")
            liftIO (print (extractValue c))
            return [c]

expression :: ParsecT [Token] Memory IO(Token)
expression = do
                try $ do
                    x1 <- exprN1
                    op <- (plusToken <|> minusToken)
                    x2 <- exprN1
                    return (eval x1 op x2)
                <|>
                do
                    x1 <-exprN1
                    return (x1)
                <|>
                do
                    i <- idToken
                    s <- getState
                    return (symtableLookup i s)

exprN1 :: ParsecT [Token] Memory IO(Token)
exprN1 = do
            try $ do
                x1 <- exprN2
                op <- (multiplyToken <|> divideToken <|> modToken)
                x2 <- exprN2
                return (eval x1 op x2)
            <|>
            do
                x1 <- exprN2
                return (x1)

exprN2 :: ParsecT [Token] Memory IO(Token)
exprN2 = do
            try $ do
                x1 <- exprN3
                op <- (powerToken)
                x2 <- exprN3
                return (eval x1 op x2)
            <|>
            do
                x1 <- exprN3
                return (x1)

exprN3 :: ParsecT [Token] Memory IO(Token)
exprN3 = do
            try $ do
                a <- openRoundToken
                b <- expression
                c <- closeRoundToken
                return (b)
            <|>
            do
                a <-(valueFloatToken <|> valueIntToken)
                return (a)

-- funções para o avaliador de expressões

eval :: Token -> Token -> Token -> Token
eval (ValueInt x p) (Plus _) (ValueInt y _) = ValueInt (x + y) p
eval (ValueInt x p) (Minus _) (ValueInt y _) = ValueInt (x - y) p
eval (ValueInt x p) (Multiply _) (ValueInt y _) = ValueInt (x * y) p
eval (ValueInt x p) (Divide _) (ValueInt y _) = ValueInt (div x y) p
eval (ValueInt x p) (Mod _) (ValueInt y _) = ValueInt (mod x y) p
eval (ValueInt x p) (Power _) (ValueInt y _) = ValueInt (x ^ y) p

-- funções para verificação de tipos

getType :: Token -> Memory -> Token
getType _ [] = error "Variable not found"
getType (ID id1 p1) ((ID id2 _, value):t) = if id1 == id2 then value
                                             else getType (ID id1 p1) t

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