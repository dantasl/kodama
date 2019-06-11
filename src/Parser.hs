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
            _ <- (letToken <?> "let")
            id <- (idToken <?> "identifier")
            _ <- (assignmentToken <?> "=")
            value <- expression
            _ <- (semiColonToken <?> ";")
            updateState(symtableInsert (id, value))
            return [id, value]

assign :: ParsecT [Token] Memory IO([Token])
assign = do
            id <- idToken
            _ <- assignmentToken
            value <- expression
            _ <- semiColonToken
            updateState(symtableUpdate (id, value))
            return [id, value]

ioStm :: ParsecT [Token] Memory IO([Token])
ioStm = do 
            a <- printStm <|> printlnStm <|> readStm
            return a

printStm :: ParsecT [Token] Memory IO([Token])
printStm = do
            _ <- (printToken <?> "print")
            _ <- (openRoundToken <?> "(")
            e <- expression
            _ <- (closeRoundToken <?> ")")
            _ <- (semiColonToken <?> ";")
            liftIO (putStr (show (extractValue e)))
            return [e]

printlnStm :: ParsecT [Token] Memory IO([Token])
printlnStm = do
            _ <- (printlnToken <?> "println")
            _ <- (openRoundToken <?> "(")
            e <- expression
            _ <- (closeRoundToken <?> ")")
            _ <- (semiColonToken <?> ";")
            liftIO (print (extractValue e))
            return [e]

readStm :: ParsecT [Token] Memory IO([Token])
readStm = do
            _ <- (readToken <?> "read")
            _ <- (openRoundToken <?> "(")
            _ <- (closeRoundToken <?> ")")
            _ <- (semiColonToken <?> ";")
            i <- liftIO (getLine)
            return []

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