{
  module Lexer where
  import System.IO
  import System.IO.Unsafe 
}

%wrapper "posn"

$digit      = [0-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$graphic    = $printable # $white
$alpha = [a-zA-F]
tokens :-
  $white+                         ;
  "--".*                          ;
  
  "(" { \p s -> OpenRound p}
  ")" { \p s -> CloseRound p}
  "[" { \p s -> OpenSquare p}
  "]" { \p s -> CloseSquare p}
  "{" { \p s -> OpenCurly p}
  "}" { \p s -> CloseCurly p}
  ":" { \p s -> Colon p}
  ";" { \p s -> SemiColon p}
  "," { \p s -> Comma p}
  
  "+=" { \p s -> PlusAssigment p}
  "-=" { \p s -> MinusAssigment p}
  "/=" { \p s ->  DivideAssigment p}
  "*=" { \p s -> MultiplyAssigment p}
  "=" { \p s -> Assigment p}
  "+" { \p s -> Plus p}
  "-" { \p s -> Minus p}
  "*" { \p s -> Multiply p}
  "**" { \p s -> Power p}
  "/" { \p s -> Divide p}
  
  while { \p s -> While p}
  for { \p s -> For p}
  return { \p s -> Return p}
  break { \p s -> Break p}
  pass { \p s -> Pass p}
  switch { \p s -> Switch p}
  case { \p s -> Case p}

  if { \p s -> If p}
  else { \p s -> Else p}
  else if { \p s -> ElseIf p}
  and { \p s -> And p}
  or { \p s -> Or p}
  not { \p s -> Not p}
  "!=" { \p s -> NotEqual p}
  "==" { \p s -> Equal p}
  "<=" { \p s -> LessEqual p}
  "<" { \p s -> Less p}
  ">=" { \p s -> MoreEqual p}
  ">" { \p s -> More p}

  uint8 { \p s -> TypeUInt8 p}
  uint16 { \p s -> TypeUInt16 p}
  uint32 { \p s -> TypeUInt32 p}
  uint64 { \p s -> TypeUInt64 p}
  int8 { \p s -> TypeInt8 p}
  int16 { \p s -> TypeInt16 p}
  int32 { \p s -> TypeInt32 p}
  int64 { \p s -> TypeInt64 p}
  flaot16 { \p s -> TypeFloat16 p}
  float32 { \p s -> TypeFloat32 p}
  float64 { \p s -> TypeFloat64 p}
  float128 { \p s -> TypeFloat128 p}
  string { \p s -> TypeString p}
  bool { \p s -> TypeBoolean p}

  let { \p s -> Let p }
  const { \p s -> Const p }
  $digit+	{ \p s -> ValueInt p (read s) }
  $digit+\.$digit+ { \p s -> ValueFloat p (read s) }
  $alpha($alpha # $digit # \_)*	  { \p s -> ID p (read s) }
  \" ($graphic # \")*  \"  { \p s -> ValueString p (read s) }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token
-- Some action helpers:

-- The token type:
data Token =
  OpenRound AlexPosn  |
  CloseRound AlexPosn  |
  OpenSquare AlexPosn  |
  CloseSquare AlexPosn  |
  OpenCurly AlexPosn  |
  CloseCurly AlexPosn  |
  Colon AlexPosn  |
  SemiColon AlexPosn  |
  Comma AlexPosn  |
  PlusAssigment AlexPosn  |
  MinusAssigment AlexPosn  |
  DivideAssigment AlexPosn  |
  MultiplyAssigment AlexPosn  |
  Assigment AlexPosn  |
  Plus AlexPosn  |
  Minus AlexPosn  |
  Multiply AlexPosn  |
  Power AlexPosn  |
  Divide AlexPosn  |
  While AlexPosn  |
  For AlexPosn  |
  Return AlexPosn  |
  Break AlexPosn  |
  Pass AlexPosn  |
  Switch AlexPosn  |
  Case AlexPosn  |
  If AlexPosn  |
  Else AlexPosn  |
  ElseIf AlexPosn  |
  And AlexPosn  |
  Or AlexPosn  |
  Not AlexPosn  |
  NotEqual AlexPosn  |
  Equal AlexPosn  |
  LessEqual AlexPosn  |
  Less AlexPosn  |
  MoreEqual AlexPosn  |
  TypeUInt8 AlexPosn  |
  TypeUInt16 AlexPosn  |
  TypeUInt32 AlexPosn  |
  TypeUInt64 AlexPosn  |
  TypeInt8 AlexPosn  |
  TypeInt16 AlexPosn  |
  TypeInt32 AlexPosn  |
  TypeInt64 AlexPosn  |
  TypeFloat16 AlexPosn  |
  TypeFloat32 AlexPosn  |
  TypeFloat64 AlexPosn  |
  TypeFloat128 AlexPosn  |
  TypeString AlexPosn  |
  TypeBoolean AlexPosn  |
  Let AlexPosn        |
  Const AlexPosn |
  ID AlexPosn  String |
  ValueInt AlexPosn Int |
  ValueFloat AlexPosn Double |
  ValueString AlexPosn String 
  deriving (Eq,Show)


getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
