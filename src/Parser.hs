module Main (main) where

import Lexer
import Tokens
import Values
import Memory
import Text.Parsec
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe

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
            d <- expression
            e <- (semiColonToken <?> ";")
            updateState(symtableInsert (b, d))
            return [b,d]

assign :: ParsecT [Token] Memory IO([Token])
assign = do
            a <- idToken
            b <- assignmentToken
            c <- expression
            d <- semiColonToken
            updateState(symtableUpdate (a, c))
            return [a,c]

ioStm :: ParsecT [Token] Memory IO([Token])
ioStm = do 
            a <- printStm <|> printlnStm <|> readStm
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
            a <- (printlnToken <?> "println")
            b <- (openRoundToken <?> "(")
            c <- expression
            d <- (closeRoundToken <?> ")")
            e <- (semiColonToken <?> ";")
            liftIO (print (extractValue c))
            return [c]

readStm :: ParsecT [Token] Memory IO([Token])
readStm = do
            a <- (readToken <?> "read")
            b <- (openRoundToken <?> "(")
            c <- (closeRoundToken <?> ")")
            d <- (semiColonToken <?> ";")
            i <- liftIO (getLine)
            return [a]

expression :: ParsecT [Token] Memory IO(Token)
expression = do
                try $ do
                    a <- mathExpression
                    return (a)
                <|>
                (try $ do
                    a <- booleanExpression
                    return (a))
                <|>
                do
                    a <- valueStringToken
                    return (a)

mathExpression :: ParsecT [Token] Memory IO(Token)
mathExpression = do
                try $ do
                    x1 <- exprN0
                    op <- (plusToken)
                    x2 <- mathExpression
                    return (eval x1 op x2)
                <|>
                do
                    x1 <- exprN0
                    return (x1)

exprN0 :: ParsecT [Token] Memory IO(Token)
exprN0 = do
            try $ do
                x1 <- exprN1
                op <- (minusToken)
                x2 <- exprN0
                return (eval x1 op x2)
            <|>
            do
                x1 <- exprN1
                return (x1)

exprN1 :: ParsecT [Token] Memory IO(Token)
exprN1 = do
            try $ do
                x1 <- exprN2
                op <- (multiplyToken <|> divideToken <|> modToken)
                x2 <- exprN1
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
                x2 <- exprN2
                return (eval x1 op x2)
            <|>
            do
                x1 <- exprN3
                return (x1)

exprN3 :: ParsecT [Token] Memory IO(Token)
exprN3 = do
            try $ do
                a <- minusToken
                b <- exprN3
                return (unaryEval a b)
            <|>
            (try $ do
                a <- openRoundToken
                b <- mathExpression
                c <- closeRoundToken
                return (b))
            <|>
            do
                i <- idToken
                s <- getState
                return (symtableLookup i s)
            <|>
            do
                a <- (valueFloatToken <|> valueIntToken)
                return (a)

booleanExpression :: ParsecT [Token] Memory IO(Token)
booleanExpression = do
                        try $ do
                            x1 <- exprB0
                            op <- (andToken <|> orToken)
                            x2 <- booleanExpression
                            return (eval x1 op x2)
                        <|>
                        do
                            x1 <- exprB0
                            return (x1)

exprB0 :: ParsecT [Token] Memory IO(Token)
exprB0 = do
            try $ do
                a <- notToken
                b <- exprB0
                return (unaryEval a b)
            <|>
            (try $ do
                a <- openRoundToken
                b <- booleanExpression
                c <- closeRoundToken
                return (b))
            <|>
            do
                i <- idToken
                s <- getState
                return (symtableLookup i s)
            <|>
            do
                a <- (valueBoolToken)
                return (a)

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

main' :: String -> IO ()
main' filepath = case unsafePerformIO (parser (getTokens filepath)) of
            { Left err -> print err;
                Right ans -> print ans
            }