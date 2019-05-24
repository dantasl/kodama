{
  module Lexer where
}

%wrapper "posn"

$digit    = [0-9]
$graphic  = $printable # $white
$alpha    = [a-zA-Z]

tokens :-
  $white+                         ;
  "--".*                          ;
  
  "(" { \p s -> OpenRound (getLineColumn p) }
  ")" { \p s -> CloseRound (getLineColumn p) }
  "[" { \p s -> OpenSquare (getLineColumn p) }
  "]" { \p s -> CloseSquare (getLineColumn p) }
  "{" { \p s -> OpenCurly (getLineColumn p) }
  "}" { \p s -> CloseCurly (getLineColumn p) }
  ":" { \p s -> Colon (getLineColumn p) }
  ";" { \p s -> SemiColon (getLineColumn p) }
  "," { \p s -> Comma (getLineColumn p) }
  
  "+=" { \p s -> PlusAssignment (getLineColumn p) }
  "-=" { \p s -> MinusAssignment (getLineColumn p) }
  "/=" { \p s ->  DivideAssignment (getLineColumn p) }
  "*=" { \p s -> MultiplyAssignment (getLineColumn p) }
  "=" { \p s -> Assignment (getLineColumn p) }
  "+" { \p s -> Plus (getLineColumn p) }
  "-" { \p s -> Minus (getLineColumn p) }
  "*" { \p s -> Multiply (getLineColumn p) }
  "**" { \p s -> Power (getLineColumn p) }
  "/" { \p s -> Divide (getLineColumn p) }
  
  while { \p s -> While (getLineColumn p) }
  for { \p s -> For (getLineColumn p) }
  return { \p s -> Return (getLineColumn p) }
  break { \p s -> Break (getLineColumn p) }
  pass { \p s -> Pass (getLineColumn p) }
  switch { \p s -> Switch (getLineColumn p) }
  case { \p s -> Case (getLineColumn p) }

  print { \p s -> Print (getLineColumn p) }
  println { \p s -> Println (getLineColumn p) }
  read { \p s -> Read (getLineColumn p) }

  if { \p s -> If (getLineColumn p) }
  else { \p s -> Else (getLineColumn p) }
  else if { \p s -> ElseIf (getLineColumn p) }
  and { \p s -> And (getLineColumn p) }
  or { \p s -> Or (getLineColumn p) }
  not { \p s -> Not (getLineColumn p) }
  "!=" { \p s -> NotEqual (getLineColumn p) }
  "==" { \p s -> Equal (getLineColumn p) }
  "<=" { \p s -> LessEqual (getLineColumn p) }
  "<" { \p s -> Less (getLineColumn p) }
  ">=" { \p s -> MoreEqual (getLineColumn p) }
  ">" { \p s -> More (getLineColumn p) }

  uint8 { \p s -> TypeUInt8 (getLineColumn p) }
  uint16 { \p s -> TypeUInt16 (getLineColumn p) }
  uint32 { \p s -> TypeUInt32 (getLineColumn p) }
  uint64 { \p s -> TypeUInt64 (getLineColumn p) }
  int8 { \p s -> TypeInt8 (getLineColumn p) }
  int16 { \p s -> TypeInt16 (getLineColumn p) }
  int32 { \p s -> TypeInt32 (getLineColumn p) }
  int64 { \p s -> TypeInt64 (getLineColumn p) }
  flaot16 { \p s -> TypeFloat16 (getLineColumn p) }
  float32 { \p s -> TypeFloat32 (getLineColumn p) }
  float64 { \p s -> TypeFloat64 (getLineColumn p) }
  float128 { \p s -> TypeFloat128 (getLineColumn p) }
  string { \p s -> TypeString (getLineColumn p) }
  bool { \p s -> TypeBoolean (getLineColumn p) }
  let { \p s -> Let (getLineColumn p) }
  const { \p s -> Const (getLineColumn p) }
  true {\p s -> ValueBool True (getLineColumn p) }
  false {\p s -> ValueBool False (getLineColumn p) }
  $digit+	{ \p s -> ValueInt (read s) (getLineColumn p) }
  $digit+\.$digit+ { \p s -> ValueFloat (read s) (getLineColumn p) }
  $alpha[$alpha $digit \_]*	  { \p s -> ID s (getLineColumn p) }
  \" ($graphic # \")*  \"  { \p s -> ValueString (read s) (getLineColumn p) }

{

-- The token type:
data Token = OpenRound (Int, Int)
           | CloseRound (Int, Int)
           | OpenSquare (Int, Int)
           | CloseSquare (Int, Int)
           | OpenCurly (Int, Int)
           | CloseCurly (Int, Int)
           | Colon (Int, Int)
           | SemiColon (Int, Int)
           | Comma (Int, Int)
           | PlusAssignment (Int, Int)
           | MinusAssignment (Int, Int)
           | DivideAssignment (Int, Int)
           | MultiplyAssignment (Int, Int)
           | Assignment (Int, Int)
           | Plus (Int, Int)
           | Minus (Int, Int)
           | Multiply (Int, Int)
           | Power (Int, Int)
           | Divide (Int, Int)
           | While (Int, Int)
           | For (Int, Int)
           | Return (Int, Int)
           | Break (Int, Int)
           | Pass (Int, Int)
           | Switch (Int, Int)
           | Case (Int, Int)
           | Print (Int, Int)
           | Println (Int, Int)
           | Read (Int, Int)
           | If (Int, Int)
           | Else (Int, Int)
           | ElseIf (Int, Int)
           | And (Int, Int)
           | Or (Int, Int)
           | Not (Int, Int)
           | NotEqual (Int, Int)
           | Equal (Int, Int)
           | LessEqual (Int, Int)
           | Less (Int, Int)
           | MoreEqual (Int, Int)
           | More (Int, Int)
           | TypeUInt8 (Int, Int)
           | TypeUInt16 (Int, Int)
           | TypeUInt32 (Int, Int)
           | TypeUInt64 (Int, Int)
           | TypeInt8 (Int, Int)
           | TypeInt16 (Int, Int)
           | TypeInt32 (Int, Int)
           | TypeInt64 (Int, Int)
           | TypeFloat16 (Int, Int)
           | TypeFloat32 (Int, Int)
           | TypeFloat64 (Int, Int)
           | TypeFloat128 (Int, Int)
           | TypeString (Int, Int)
           | TypeBoolean (Int, Int)
           | Let (Int, Int)
           | Const (Int, Int)
           | ID String (Int, Int)
           | ValueBool Bool (Int, Int)
           | ValueInt Int (Int, Int)
           | ValueFloat Double (Int, Int)
           | ValueString String (Int, Int)
             deriving (Eq,Show)

getLineColumn (AlexPn _ l c) = (l, c)

scanTokens s = alexScanTokens s

}