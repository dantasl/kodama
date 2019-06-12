module Main (main) where

import Lexer
import Tokens
import Values
import Memory
import Interpreter as Interpreter
import Text.Parsec
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe

-- parsers para os não-terminais

program :: ParsecT [Token] Memory IO([Statement])
program = do
            a <- stmts
            eof
            return (a)

stmts :: ParsecT [Token] Memory IO([Statement])
stmts = do
          a <- many (varDeclaration <|> assign <|> ioStm <|> ifStm)
          return (a)

varDeclaration :: ParsecT [Token] Memory IO(Statement)
varDeclaration = do
            _ <- (letToken <?> "let")
            id <- (idToken <?> "identifier")
            _ <- (assignmentToken <?> "=")
            value <- expression
            _ <- (semiColonToken <?> ";")
            updateState(symtableInsert (id, extractToken value))
            return (Interpreter.VarDeclaration id value)

assign :: ParsecT [Token] Memory IO(Statement)
assign = do
            id <- idToken
            _ <- assignmentToken
            value <- expression
            _ <- semiColonToken
            updateState(symtableUpdate (id, extractToken value))
            return (Interpreter.Assignment id value)

ioStm :: ParsecT [Token] Memory IO(Statement)
ioStm = do 
            a <- printStm <|> printlnStm <|> readStm
            return a

printStm :: ParsecT [Token] Memory IO(Statement)
printStm = do
            _ <- (printToken <?> "print")
            _ <- (openRoundToken <?> "(")
            e <- expression
            _ <- (closeRoundToken <?> ")")
            _ <- (semiColonToken <?> ";")
            return (Interpreter.Print e)

printlnStm :: ParsecT [Token] Memory IO(Statement)
printlnStm = do
            _ <- (printlnToken <?> "println")
            _ <- (openRoundToken <?> "(")
            e <- expression
            _ <- (closeRoundToken <?> ")")
            _ <- (semiColonToken <?> ";")
            return (Interpreter.Println e)

readStm :: ParsecT [Token] Memory IO(Statement)
readStm = do
            id <- idToken
            _ <- assignmentToken
            _ <- (readToken <?> "read")
            _ <- (openRoundToken <?> "(")
            _ <- (closeRoundToken <?> ")")
            _ <- (semiColonToken <?> ";")
            return (Interpreter.Read id)

expression :: ParsecT [Token] Memory IO(Expression)
expression = do
                try $ do
                    a <- mathExpression
                    return (MathExpression a)
                <|>
                (try $ do
                    a <- booleanExpression
                    return (BooleanExpression a))
                <|>
                do
                    a <- valueStringToken
                    return (StringExpression a)

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

ifStm :: ParsecT [Token] Memory IO(Statement)
ifStm = do
            _ <- (ifToken <?> "if")
            b <- (booleanExpression <?> "expression")
            _ <- (openCurlyToken <?> "{")
            s1 <- stmts
            _ <- (closeCurlyToken <?> "}")
            _ <- (elseToken <?> "else")
            _ <- (openCurlyToken <?> "{")
            s2 <- stmts
            _ <- (closeCurlyToken <?> "}")
            let bs = BooleanExpression b
            let cs1 = Chain s1
            let cs2 = Chain s2
            return (Interpreter.If bs cs1 cs2)

-- invocação do parser para o símbolo de partida

printTokens = do contents <- readFile "exemplo.kod"
                 print (scanTokens contents)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}

parser :: [Token] -> IO (Either ParseError [Statement])
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