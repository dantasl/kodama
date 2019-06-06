module Memory where

{- Módulo responsável pela definição e operações sobre
a mémoria dos programas em execução -}

import Lexer
import Tokens

type MemoryCell = (Token,Token)
type Memory = [MemoryCell]

symtableLookup :: Token -> Memory -> Maybe Token
symtableLookup _ [] = fail "variable not found"
symtableLookup tok symtable = lookup tok symtable

symtableInsert :: MemoryCell -> Memory -> Memory
symtableInsert symbol []  = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]

symtableUpdate :: MemoryCell -> Memory -> Memory
symtableUpdate _ [] = fail "variable not found"
symtableUpdate (ID id1 p1, v1) ((ID id2 p2, v2):t) = 
                               if id1 == id2 then (ID id1 p2, v1) : t
                               else (ID id2 p2, v2) : symtableUpdate (ID id1 p1, v1) t

symtableRemove :: MemoryCell -> Memory -> Memory
symtableRemove _ [] = fail "variable not found"
symtableRemove (id1, v1) ((id2, v2):t) = 
                                if id1 == id2 then t
                                else (id2, v2) : symtableRemove (id1, v1) t