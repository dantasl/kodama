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

compatible :: Token -> Token -> Bool
compatible (ValueInt _ _) (ValueInt _ _) = True
compatible _ _ = False

getDefaultValue :: Token -> Token
getDefaultValue (TypeInt8 (l, c)) = ValueInt 0 (l, c)