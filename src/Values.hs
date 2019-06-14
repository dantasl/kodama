module Values where

{- Módulo responsável por definir os tipos de valores 
da linguagem, e as operações sobre esses valores -}

import Lexer
import Tokens

data Value = Bool Bool
           | String String
           | Int Int
           | Float Double

instance Show Value where
    show (Bool x)     = show x
    show (String x)   = show x
    show (Int x)      = show x
    show (Float x)    = show x

extractValue :: Token -> Value
extractValue (ValueBool x p) = Bool x
extractValue (ValueString x p) = String x
extractValue (ValueInt x p) = Int x
extractValue (ValueFloat x p) = Float x

extractBooleanValue :: Value -> Bool
extractBooleanValue (Bool x) = x

compatible :: Token -> Token -> Bool
compatible (ValueBool _ _) (ValueBool _ _) = True
compatible (ValueString _ _) (ValueString _ _) = True
compatible (ValueInt _ _) (ValueInt _ _) = True
compatible (ValueFloat _ _) (ValueFloat _ _) = True
compatible _ _ = False

convertStringToValue :: Token -> String -> Token
convertStringToValue (ValueBool _ p) s = (ValueBool (read s) p)
convertStringToValue (ValueString _ p) s = (ValueString s p)
convertStringToValue (ValueInt _ p) s = (ValueInt (read s) p)
convertStringToValue (ValueFloat _ p) s = (ValueFloat (read s) p)

unaryEval :: Token -> Token -> Token
unaryEval (Minus _) (ValueInt x p) = ValueInt (-x) p
unaryEval (Minus _) (ValueFloat x p) = ValueFloat (-x) p

unaryEval (Not _) (ValueBool x p) = ValueBool (not x) p

unaryEval _ _ = error "cannot evaluate this expression"

eval :: Token -> Token -> Token -> Token
eval (ValueInt x p) (Plus _) (ValueInt y _) = ValueInt (x + y) p
eval (ValueInt x p) (Plus _) (ValueFloat y _) = ValueFloat ((fromIntegral x) + y) p
eval (ValueFloat x p) (Plus _) (ValueInt y _) = ValueFloat (x + (fromIntegral y)) p
eval (ValueFloat x p) (Plus _) (ValueFloat y _) = ValueFloat (x + y) p

eval (ValueInt x p) (Minus _) (ValueInt y _) = ValueInt (x - y) p
eval (ValueInt x p) (Minus _) (ValueFloat y _) = ValueFloat ((fromIntegral x) - y) p
eval (ValueFloat x p) (Minus _) (ValueInt y _) = ValueFloat (x - (fromIntegral y)) p
eval (ValueFloat x p) (Minus _) (ValueFloat y _) = ValueFloat (x - y) p

eval (ValueInt x p) (Multiply _) (ValueInt y _) = ValueInt (x * y) p
eval (ValueInt x p) (Multiply _) (ValueFloat y _) = ValueFloat ((fromIntegral x) * y) p
eval (ValueFloat x p) (Multiply _) (ValueInt y _) = ValueFloat (x * (fromIntegral y)) p
eval (ValueFloat x p) (Multiply _) (ValueFloat y _) = ValueFloat (x * y) p

eval (ValueInt x p) (Divide _) (ValueInt y _) = ValueInt (div x y) p
eval (ValueInt x p) (Divide _) (ValueFloat y _) = ValueFloat ((fromIntegral x) / y) p
eval (ValueFloat x p) (Divide _) (ValueInt y _) = ValueFloat (x / (fromIntegral y)) p
eval (ValueFloat x p) (Divide _) (ValueFloat y _) = ValueFloat (x / y) p

eval (ValueInt x p) (Mod _) (ValueInt y _) = ValueInt (mod x y) p

eval (ValueInt x p) (Power _) (ValueInt y _) = ValueInt (x ^ y) p
eval (ValueInt x p) (Power _) (ValueFloat y _) = ValueFloat ((fromIntegral x) ** y) p
eval (ValueFloat x p) (Power _) (ValueInt y _) = ValueFloat (x ^^ y) p
eval (ValueFloat x p) (Power _) (ValueFloat y _) = ValueFloat (x ** y) p

eval (ValueBool x p) (And _) (ValueBool y _) = ValueBool (x && y) p
eval (ValueBool x p) (Or _) (ValueBool y _) = ValueBool (x || y) p

eval (ValueInt x p) (Less _) (ValueInt y _) = ValueBool (x < y) p
eval (ValueInt x p) (Less _) (ValueFloat y _) = ValueBool ((fromIntegral x) < y) p
eval (ValueFloat x p) (Less _) (ValueInt y _) = ValueBool (x < (fromIntegral y)) p
eval (ValueFloat x p) (Less _) (ValueFloat y _) = ValueBool (x < y) p

eval (ValueInt x p) (LessEqual _) (ValueInt y _) = ValueBool (x <= y) p
eval (ValueInt x p) (LessEqual _) (ValueFloat y _) = ValueBool ((fromIntegral x) <= y) p
eval (ValueFloat x p) (LessEqual _) (ValueInt y _) = ValueBool (x <= (fromIntegral y)) p
eval (ValueFloat x p) (LessEqual _) (ValueFloat y _) = ValueBool (x <= y) p

eval (ValueInt x p) (More _) (ValueInt y _) = ValueBool (x > y) p
eval (ValueInt x p) (More _) (ValueFloat y _) = ValueBool ((fromIntegral x) > y) p
eval (ValueFloat x p) (More _) (ValueInt y _) = ValueBool (x > (fromIntegral y)) p
eval (ValueFloat x p) (More _) (ValueFloat y _) = ValueBool (x > y) p

eval (ValueInt x p) (MoreEqual _) (ValueInt y _) = ValueBool (x >= y) p
eval (ValueInt x p) (MoreEqual _) (ValueFloat y _) = ValueBool ((fromIntegral x) >= y) p
eval (ValueFloat x p) (MoreEqual _) (ValueInt y _) = ValueBool (x >= (fromIntegral y)) p
eval (ValueFloat x p) (MoreEqual _) (ValueFloat y _) = ValueBool (x >= y) p

eval (ValueString x p) (Plus _) (ValueString y _) = ValueString (x ++ y) p
eval (ValueString x p) (Plus _) (ValueInt y _) = ValueString (x ++ (show y)) p
eval (ValueString x p) (Plus _) (ValueFloat y _) = ValueString (x ++ (show y)) p
eval (ValueString x p) (Plus _) (ValueBool y _) = ValueString (x ++ (show y)) p
eval (ValueInt x p) (Plus _) (ValueString y _) = ValueString ((show x) ++ y) p
eval (ValueFloat x p) (Plus _) (ValueString y _) = ValueString ((show x) ++ y) p
eval (ValueBool x p) (Plus _) (ValueString y _) = ValueString ((show x) ++ y) p

eval _ _ _ = error "cannot evaluate this expression"

getDefaultValue :: Token -> Token
getDefaultValue (TypeInt8 (l, c)) = ValueInt 0 (l, c)