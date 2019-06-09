module Tokens where

import Text.Parsec
import Lexer

-- parsers para os tokens

letToken :: ParsecT [Token] st IO (Token)
letToken = tokenPrim show update_pos get_token where
    get_token (Let p) = Just (Let p)
    get_token _           = Nothing

idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
    get_token (ID x p) = Just (ID x p)
    get_token _        = Nothing

assignmentToken :: ParsecT [Token] st IO (Token)
assignmentToken = tokenPrim show update_pos get_token where
    get_token (Assignment p) = Just (Assignment p)
    get_token _           = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
    get_token (SemiColon p) = Just (SemiColon p)
    get_token _           = Nothing

openRoundToken :: ParsecT [Token] st IO (Token)
openRoundToken = tokenPrim show update_pos get_token where
    get_token (OpenRound p) = Just (OpenRound p)
    get_token _           = Nothing

closeRoundToken :: ParsecT [Token] st IO (Token)
closeRoundToken = tokenPrim show update_pos get_token where
    get_token (CloseRound p) = Just (CloseRound p)
    get_token _           = Nothing

printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
    get_token (Print p) = Just (Print p)
    get_token _           = Nothing

printlnToken :: ParsecT [Token] st IO (Token)
printlnToken = tokenPrim show update_pos get_token where
    get_token (Println p) = Just (Println p)
    get_token _           = Nothing

readToken :: ParsecT [Token] st IO (Token)
readToken = tokenPrim show update_pos get_token where
    get_token (Read p) = Just (Read p)
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

plusToken :: ParsecT [Token] st IO (Token)
plusToken = tokenPrim show update_pos get_token where
    get_token (Plus p) = Just (Plus p)
    get_token _           = Nothing

minusToken :: ParsecT [Token] st IO (Token)
minusToken = tokenPrim show update_pos get_token where
    get_token (Minus p) = Just (Minus p)
    get_token _           = Nothing

multiplyToken :: ParsecT [Token] st IO (Token)
multiplyToken = tokenPrim show update_pos get_token where
    get_token (Multiply p) = Just (Multiply p)
    get_token _           = Nothing

powerToken :: ParsecT [Token] st IO (Token)
powerToken = tokenPrim show update_pos get_token where
    get_token (Power p) = Just (Power p)
    get_token _           = Nothing

divideToken :: ParsecT [Token] st IO (Token)
divideToken = tokenPrim show update_pos get_token where
    get_token (Divide p) = Just (Divide p)
    get_token _           = Nothing

modToken :: ParsecT [Token] st IO (Token)
modToken = tokenPrim show update_pos get_token where
    get_token (Mod p) = Just (Mod p)
    get_token _           = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  