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

plusAssignmentToken :: ParsecT [Token] st IO (Token)
plusAssignmentToken = tokenPrim show update_pos get_token where
    get_token (PlusAssignment p) = Just (PlusAssignment p)
    get_token _           = Nothing

minusAssignmentToken :: ParsecT [Token] st IO (Token)
minusAssignmentToken = tokenPrim show update_pos get_token where
    get_token (MinusAssignment p) = Just (MinusAssignment p)
    get_token _           = Nothing

divideAssignmentToken :: ParsecT [Token] st IO (Token)
divideAssignmentToken = tokenPrim show update_pos get_token where
    get_token (DivideAssignment p) = Just (DivideAssignment p)
    get_token _           = Nothing

multiplyAssignmentToken :: ParsecT [Token] st IO (Token)
multiplyAssignmentToken = tokenPrim show update_pos get_token where
    get_token (MultiplyAssignment p) = Just (MultiplyAssignment p)
    get_token _           = Nothing

assignmentToken :: ParsecT [Token] st IO (Token)
assignmentToken = tokenPrim show update_pos get_token where
    get_token (Assignment p) = Just (Assignment p)
    get_token _           = Nothing

colonToken :: ParsecT [Token] st IO (Token)
colonToken = tokenPrim show update_pos get_token where
    get_token (Colon p) = Just (Colon p)
    get_token _           = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
    get_token (SemiColon p) = Just (SemiColon p)
    get_token _           = Nothing

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
        get_token (Comma p) = Just (Comma p)
        get_token _           = Nothing

openRoundToken :: ParsecT [Token] st IO (Token)
openRoundToken = tokenPrim show update_pos get_token where
    get_token (OpenRound p) = Just (OpenRound p)
    get_token _           = Nothing

closeRoundToken :: ParsecT [Token] st IO (Token)
closeRoundToken = tokenPrim show update_pos get_token where
    get_token (CloseRound p) = Just (CloseRound p)
    get_token _           = Nothing

openSquareToken :: ParsecT [Token] st IO (Token)
openSquareToken = tokenPrim show update_pos get_token where
    get_token (OpenSquare p) = Just (OpenSquare p)
    get_token _           = Nothing

closeSquareToken :: ParsecT [Token] st IO (Token)
closeSquareToken = tokenPrim show update_pos get_token where
    get_token (CloseSquare p) = Just (CloseSquare p)
    get_token _           = Nothing

openCurlyToken :: ParsecT [Token] st IO (Token)
openCurlyToken = tokenPrim show update_pos get_token where
    get_token (OpenCurly p) = Just (OpenCurly p)
    get_token _           = Nothing

closeCurlyToken :: ParsecT [Token] st IO (Token)
closeCurlyToken = tokenPrim show update_pos get_token where
    get_token (CloseCurly p) = Just (CloseCurly p)
    get_token _           = Nothing

ifToken :: ParsecT [Token] st IO (Token)
ifToken = tokenPrim show update_pos get_token where
    get_token (If p) = Just (If p)
    get_token _           = Nothing

elseToken :: ParsecT [Token] st IO (Token)
elseToken = tokenPrim show update_pos get_token where
    get_token (Else p) = Just (Else p)
    get_token _           = Nothing

elseIfToken :: ParsecT [Token] st IO (Token)
elseIfToken = tokenPrim show update_pos get_token where
    get_token (ElseIf p) = Just (ElseIf p)
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

andToken :: ParsecT [Token] st IO (Token)
andToken = tokenPrim show update_pos get_token where
        get_token (And p) = Just (And p)
        get_token _           = Nothing

orToken :: ParsecT [Token] st IO (Token)
orToken = tokenPrim show update_pos get_token where
    get_token (Or p) = Just (Or p)
    get_token _           = Nothing

notToken :: ParsecT [Token] st IO (Token)
notToken = tokenPrim show update_pos get_token where
    get_token (Not p) = Just (Not p)
    get_token _           = Nothing

notEqualToken :: ParsecT [Token] st IO (Token)
notEqualToken = tokenPrim show update_pos get_token where
    get_token (NotEqual p) = Just (NotEqual p)
    get_token _           = Nothing

equalToken :: ParsecT [Token] st IO (Token)
equalToken = tokenPrim show update_pos get_token where
    get_token (Equal p) = Just (Equal p)
    get_token _           = Nothing

lessEqualToken :: ParsecT [Token] st IO (Token)
lessEqualToken = tokenPrim show update_pos get_token where
    get_token (LessEqual p) = Just (LessEqual p)
    get_token _           = Nothing

lessToken :: ParsecT [Token] st IO (Token)
lessToken = tokenPrim show update_pos get_token where
    get_token (Less p) = Just (Less p)
    get_token _           = Nothing

moreEqualToken :: ParsecT [Token] st IO (Token)
moreEqualToken = tokenPrim show update_pos get_token where
    get_token (MoreEqual p) = Just (MoreEqual p)
    get_token _           = Nothing

moreToken :: ParsecT [Token] st IO (Token)
moreToken = tokenPrim show update_pos get_token where
    get_token (More p) = Just (More p)
    get_token _           = Nothing

whileToken :: ParsecT [Token] st IO (Token)
whileToken = tokenPrim show update_pos get_token where
    get_token (While p) = Just (While p)
    get_token _           = Nothing

forToken :: ParsecT [Token] st IO (Token)
forToken = tokenPrim show update_pos get_token where
    get_token (For p) = Just (For p)
    get_token _           = Nothing

returnToken :: ParsecT [Token] st IO (Token)
returnToken = tokenPrim show update_pos get_token where
    get_token (Return p) = Just (Return p)
    get_token _           = Nothing

breakToken :: ParsecT [Token] st IO (Token)
breakToken = tokenPrim show update_pos get_token where
    get_token (Break p) = Just (Break p)
    get_token _           = Nothing

passToken :: ParsecT [Token] st IO (Token)
passToken = tokenPrim show update_pos get_token where
    get_token (Pass p) = Just (Pass p)
    get_token _           = Nothing

switchToken :: ParsecT [Token] st IO (Token)
switchToken = tokenPrim show update_pos get_token where
    get_token (Switch p) = Just (Switch p)
    get_token _           = Nothing

caseToken :: ParsecT [Token] st IO (Token)
caseToken = tokenPrim show update_pos get_token where
    get_token (Case p) = Just (Case p)
    get_token _           = Nothing

passToken :: ParsecT [Token] st IO (Token)
passToken = tokenPrim show update_pos get_token where
    get_token (Pass p) = Just (Pass p)
    get_token _           = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  