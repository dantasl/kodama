module Main (main) where

import Lexer
import Tokens
import Values
import Memory
import Interpreter as Interpreter
import Text.Parsec
import Control.Monad.State
import Control.Monad.IO.Class

import System.IO
import System.IO.Unsafe

-- parsers para os não-terminais

program :: ParsecT [Token] Memory IO([Statement])
program = do
            a <- many (choice [(try stmts) <|> (try func)])
            eof
            return (a)

stmts :: ParsecT [Token] Memory IO(Statement)
stmts = do
          a <- choice [(try ioStm), (try varDeclaration), (try assign), (try ifStm), (try whileStm)]
          return (a)

func :: ParsecT [Token] Memory IO(Statement)
func = do
         id <- (idToken <?> "identifier")
         _ <- (openRoundToken <?> "(")
         _ <- (closeRoundToken <?> ")")
         _ <- (colonToken <?> ":")
         _ <- (openCurlyToken <?> "{")
         s <- many stmts
         _ <- (closeCurlyToken <?> "}")
         let cs = Chain s
         return (Interpreter.Function id cs)

varDeclaration :: ParsecT [Token] Memory IO(Statement)
varDeclaration = do
            _ <- (letToken <?> "let")
            id <- (idToken <?> "identifier")
            _ <- (assignmentToken <?> "=")
            value <- expression
            _ <- (semiColonToken <?> ";")
            return (Interpreter.VarDeclaration id value)

assign :: ParsecT [Token] Memory IO(Statement)
assign = do
            id <- idToken
            _ <- assignmentToken
            value <- expression
            _ <- semiColonToken
            return (Interpreter.Assignment id value)

ioStm :: ParsecT [Token] Memory IO(Statement)
ioStm = do 
            a <- choice [readStm, printStm, printlnStm]
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
            i <- idToken
            _ <- assignmentToken
            _ <- (readToken <?> "read")
            _ <- (openRoundToken <?> "(")
            _ <- (closeRoundToken <?> ")")
            _ <- (semiColonToken <?> ";")
            return (Interpreter.Read i)

expression :: ParsecT [Token] Memory IO(Expression)
expression = do
                do
                    try $ do
                        a <- mathExpression
                        return (MathExpression a)
                <|>
                do
                    try $ do
                        a <- booleanExpression
                        return (BooleanExpression a)
                <|>
                do
                    try $ do
                        a <- stringExpression
                        return (StringExpression a)

stringExpression :: ParsecT [Token] Memory IO(SExpression)
stringExpression = do
                    (try $ do
                        x1 <- exprS0
                        op <- plusToken
                        x2 <- stringExpression
                        return (BinarySExpression Interpreter.Concat x1 x2))
                    <|>
                    do
                        x1 <- exprS0
                        return (x1)

exprS0 :: ParsecT [Token] Memory IO(SExpression)
exprS0 = do
            try $ do
                i <- idToken
                return (SId i)
            <|>
            do
                a <- valueStringToken
                return (SValue a)

mathExpression :: ParsecT [Token] Memory IO(MExpression)
mathExpression = do
                try $ do
                    x1 <- exprN0
                    op <- (plusToken)
                    x2 <- mathExpression
                    let m = getMathOpFromToken op
                    return (BinaryMExpression m x1 x2)
                <|>
                do
                    x1 <- exprN0
                    return (x1)

exprN0 :: ParsecT [Token] Memory IO(MExpression)
exprN0 = do
            try $ do
                x1 <- exprN1
                op <- (minusToken)
                x2 <- exprN0
                let m = getMathOpFromToken op
                return (BinaryMExpression m x1 x2)
            <|>
            do
                x1 <- exprN1
                return (x1)

exprN1 :: ParsecT [Token] Memory IO(MExpression)
exprN1 = do
            try $ do
                x1 <- exprN2
                op <- (multiplyToken <|> divideToken <|> modToken)
                x2 <- exprN1
                let m = getMathOpFromToken op
                return (BinaryMExpression m x1 x2)
            <|>
            do
                x1 <- exprN2
                return (x1)

exprN2 :: ParsecT [Token] Memory IO(MExpression)
exprN2 = do
            try $ do
                x1 <- exprN3
                op <- (powerToken)
                x2 <- exprN2
                let m = getMathOpFromToken op
                return (BinaryMExpression m x1 x2)
            <|>
            do
                x1 <- exprN3
                return (x1)

exprN3 :: ParsecT [Token] Memory IO(MExpression)
exprN3 = do
            try $ do
                _ <- minusToken
                b <- exprN3
                return (UnaryMExpression Interpreter.Negate b)
            <|>
            (try $ do
                a <- openRoundToken
                b <- mathExpression
                c <- closeRoundToken
                return (b))
            <|>
            do
                i <- idToken
                return (MId i)
            <|>
            do
                a <- (valueFloatToken <|> valueIntToken)
                return (MValue a)

booleanExpression :: ParsecT [Token] Memory IO(BExpression)
booleanExpression = do
                        try $ do
                            x1 <- exprB0
                            op <- (andToken <|> orToken)
                            x2 <- booleanExpression
                            let b = getBooleanOpFromToken op
                            return (BinaryBExpression b x1 x2)
                        <|>
                        do
                            x1 <- exprB0
                            return (x1)

booleanRelationalExpression :: ParsecT [Token] Memory IO(BExpression)
booleanRelationalExpression = do
                                x1 <- mathExpression
                                op <- (lessEqualToken <|> lessToken <|> moreEqualToken <|> moreToken)
                                x2 <- mathExpression
                                let r = getRelationalOpFromToken op
                                return (BinaryRExpression r x1 x2)

exprB0 :: ParsecT [Token] Memory IO(BExpression)
exprB0 = do
            try $ do
                _ <- notToken
                b <- exprB0
                return (UnaryBExpression Interpreter.Not b)
            <|>
            (try $ do
                _ <- openRoundToken
                b <- booleanExpression
                _ <- closeRoundToken
                return (b))
            <|>
            (try $ do
                a <- booleanRelationalExpression
                return (a))
            <|>
            (try $ do
                i <- idToken
                return (BId i))
            <|>
            do
                a <- (valueBoolToken)
                return (BValue a)

ifStm :: ParsecT [Token] Memory IO(Statement)
ifStm = do
            _ <- (ifToken <?> "if")
            b <- (booleanExpression <?> "boolean expression")
            _ <- (openCurlyToken <?> "{")
            s1 <- many stmts
            _ <- (closeCurlyToken <?> "}")
            _ <- (elseToken <?> "else")
            _ <- (openCurlyToken <?> "{")
            s2 <- many stmts
            _ <- (closeCurlyToken <?> "}")
            let bs = BooleanExpression b
            let cs1 = Chain s1
            let cs2 = Chain s2
            return (Interpreter.If bs cs1 cs2)

whileStm :: ParsecT [Token] Memory IO(Statement)
whileStm = do
                _ <- (whileToken <?> "while")
                b <- (booleanExpression <?> "boolean expression")
                _ <- (openCurlyToken <?> "{")
                s <- many stmts
                _ <- (closeCurlyToken <?> "}")
                let bs = BooleanExpression b
                let cs = Chain s
                return (Interpreter.While bs cs)

-- invocação do parser para o símbolo de partida

printTokens = do
                contents <- readFile "exemplo.kod"
                print (scanTokens contents)

printParsedFile :: String -> IO ()
printParsedFile filepath = case unsafePerformIO (parser (getTokens filepath)) of
            { Left err -> print err;
                Right ans -> print ans
            }

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                        s <- hGetContents fh;
                        return (alexScanTokens s)}

parser :: [Token] -> IO (Either ParseError [Statement])
parser tokens = runParserT program [] "Failed" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "exemplo.kod")) of
            { Left err -> print err;
              Right ans -> evalStateT (evaluate ans) []
            }

main' :: String -> IO ()
main' filepath = case unsafePerformIO (parser (getTokens filepath)) of
            { Left err -> print err;
              Right ans -> evalStateT (evaluate ans) []
            }