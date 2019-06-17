module Interpreter where

{- Módulo responsável por interpretar 
o resultado do parser -}

import qualified Lexer as Lexer
import Values
import Memory

import Control.Monad
import Control.Monad.State

data Expression = MathExpression MExpression
                | BooleanExpression BExpression
                | StringExpression SExpression
                deriving (Show)

data MathOperator = Addition 
                  | Multiplication
                  | Division
                  | Subtraction
                  | Power
                  | Mod
                  | Negate
                  deriving (Show)

data BooleanOperator = And
                     | Or
                     | Not
                     deriving (Show)

data RelationalOperator = Less
                        | LessEqual
                        | More
                        | MoreEqual
                        deriving (Show)

data StringOperator = Concat deriving (Show)

data MExpression = MValue Lexer.Token
                 | MId Lexer.Token
                 | UnaryMExpression MathOperator MExpression
                 | BinaryMExpression MathOperator MExpression MExpression
                 deriving (Show)

data BExpression = BValue Lexer.Token
                 | BId Lexer.Token
                 | UnaryBExpression BooleanOperator BExpression
                 | BinaryBExpression BooleanOperator BExpression BExpression
                 | BinaryRExpression RelationalOperator MExpression MExpression
                 deriving (Show)

data SExpression = SValue Lexer.Token
                 | SId Lexer.Token
                 | BinarySExpression StringOperator SExpression SExpression
                    deriving (Show)

data Statement = Assignment Lexer.Token Expression
               | VarDeclaration Lexer.Token Expression
               | Print Expression
               | Println Expression
               | Read Lexer.Token
               | If Expression Statement Statement
               | While Expression Statement
               | Function Lexer.Token Statement
               | Chain [Statement]
               deriving (Show)

getBooleanOpFromToken :: Lexer.Token -> BooleanOperator
getBooleanOpFromToken (Lexer.And _) = And
getBooleanOpFromToken (Lexer.Or _) = Or
getBooleanOpFromToken (Lexer.Not _) = Not

getRelationalOpFromToken :: Lexer.Token -> RelationalOperator
getRelationalOpFromToken (Lexer.Less _) = Less
getRelationalOpFromToken (Lexer.LessEqual _) = LessEqual
getRelationalOpFromToken (Lexer.More _) = More
getRelationalOpFromToken (Lexer.MoreEqual _) = MoreEqual

getMathOpFromToken :: Lexer.Token -> MathOperator
getMathOpFromToken (Lexer.Plus _) = Addition
getMathOpFromToken (Lexer.Minus _) = Subtraction
getMathOpFromToken (Lexer.Multiply _) = Multiplication
getMathOpFromToken (Lexer.Divide _) = Division
getMathOpFromToken (Lexer.Power _) = Power
getMathOpFromToken (Lexer.Mod _) = Mod

extractToken :: Memory -> Expression -> Lexer.Token
extractToken mem exp = case exp of 
                            m@(MathExpression me) -> evalExpression mem m
                            b@(BooleanExpression be) -> evalExpression mem b
                            s@(StringExpression se) -> evalExpression mem s

evaluate :: [Statement] -> StateT Memory IO ()
evaluate stmts = mapM_ evalStmts stmts

evalExpression :: Memory -> Expression -> Lexer.Token
evalExpression mem (MathExpression me) = 
    case me of
        (MValue v) -> v
        (MId i) -> symtableLookup i mem
        (UnaryMExpression op e) -> evalUnaryMExpression mem op e
        (BinaryMExpression op x1 x2) -> evalBinaryMExpression mem op x1 x2

evalExpression mem (BooleanExpression be) = 
    case be of
        (BValue v) -> v
        (BId i) -> symtableLookup i mem
        (UnaryBExpression op e) -> evalUnaryBExpression mem op e
        (BinaryBExpression op x1 x2) -> evalBinaryBExpression mem op x1 x2
        (BinaryRExpression op x1 x2) -> evalBinaryRExpression mem op x1 x2

evalExpression mem (StringExpression se) = 
    case se of
        (SValue v) -> v
        (SId i) -> symtableLookup i mem
        (BinarySExpression op x1 x2) -> evalBinarySExpression mem op x1 x2

evalUnaryMExpression :: Memory -> MathOperator -> MExpression -> Lexer.Token
evalUnaryMExpression mem Negate exp = (unaryEval (Lexer.Minus (0,0)) (evalExpression mem (MathExpression exp)))

evalBinaryMExpression :: Memory -> MathOperator -> MExpression -> MExpression -> Lexer.Token
evalBinaryMExpression mem Addition exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.Plus (0,0)) (evalExpression mem (MathExpression exp2)))
evalBinaryMExpression mem Multiplication exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.Multiply (0,0)) (evalExpression mem (MathExpression exp2)))
evalBinaryMExpression mem Division exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.Divide (0,0)) (evalExpression mem (MathExpression exp2)))
evalBinaryMExpression mem Subtraction exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.Minus (0,0)) (evalExpression mem (MathExpression exp2)))
evalBinaryMExpression mem Power exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.Power (0,0)) (evalExpression mem (MathExpression exp2)))
evalBinaryMExpression mem Mod exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.Mod (0,0)) (evalExpression mem (MathExpression exp2)))

evalUnaryBExpression :: Memory -> BooleanOperator -> BExpression -> Lexer.Token
evalUnaryBExpression mem Not exp = (unaryEval (Lexer.Not (0,0)) (evalExpression mem (BooleanExpression exp)))

evalBinaryBExpression :: Memory -> BooleanOperator -> BExpression -> BExpression -> Lexer.Token
evalBinaryBExpression mem And exp1 exp2 = (eval (evalExpression mem (BooleanExpression exp1)) (Lexer.And (0,0)) (evalExpression mem (BooleanExpression exp2)))
evalBinaryBExpression mem Or exp1 exp2 = (eval (evalExpression mem (BooleanExpression exp1)) (Lexer.Or (0,0)) (evalExpression mem (BooleanExpression exp2)))

evalBinaryRExpression :: Memory -> RelationalOperator -> MExpression -> MExpression -> Lexer.Token
evalBinaryRExpression mem Less exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.Less (0,0)) (evalExpression mem (MathExpression exp2)))
evalBinaryRExpression mem LessEqual exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.LessEqual (0,0)) (evalExpression mem (MathExpression exp2)))
evalBinaryRExpression mem More exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.More (0,0)) (evalExpression mem (MathExpression exp2)))
evalBinaryRExpression mem MoreEqual exp1 exp2 = (eval (evalExpression mem (MathExpression exp1)) (Lexer.MoreEqual (0,0)) (evalExpression mem (MathExpression exp2)))

evalBinarySExpression :: Memory -> StringOperator -> SExpression -> SExpression -> Lexer.Token
evalBinarySExpression mem Concat exp1 exp2 = (eval (evalExpression mem (StringExpression exp1)) (Lexer.Plus (0,0)) (evalExpression mem (StringExpression exp2)))

evalStmts :: Statement -> StateT Memory IO ()

evalStmts (Chain stmts) = mapM_ evalStmts stmts

evalStmts (VarDeclaration t e) = do
    mem <- get
    modify (symtableInsert (t, evalExpression mem e))

evalStmts (Assignment t e) = do
    mem <- get
    modify (symtableUpdate (t, evalExpression mem e))

evalStmts (Print e) = do
    mem <- get
    lift $ putStr $ show $ extractValue $ evalExpression mem e

evalStmts (Println e) = do
    mem <- get
    lift $ print $ extractValue $ evalExpression mem e

evalStmts (Read i) = do
    mem <- get
    let oldValue = symtableLookup i mem
    input <- lift $ getLine
    let newValue = convertStringToValue oldValue input
    modify (symtableUpdate (i, newValue))

evalStmts (If b s1 s2) = do
    mem <- get
    if extractBooleanValue $ extractValue $ extractToken mem b
        then evalStmts s1
        else evalStmts s2

evalStmts while@(While b body) = do
    mem <- get
    when (extractBooleanValue $ extractValue $ extractToken mem b) $ evalStmts body >> evalStmts while