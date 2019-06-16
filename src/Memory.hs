module Memory where

{- Módulo responsável pela definição e operações sobre
a mémoria dos programas em execução -}

import Lexer
import Tokens

type MemoryCell = (Token,Token)
type Memory = [MemoryCell]

-- funções auxiliares

extractId :: Token -> String
extractId (ID x p) = x

-- funções de acesso a memória

symtableLookup :: Token -> Memory -> Token
symtableLookup (ID id1 p1) [] = error $ "Variable " ++ id1 ++ " not found."
symtableLookup (ID id1 p1) ((ID id2 p2, value):mem) = 
                            if id1 == id2 then value
                            else symtableLookup (ID id1 p1) mem

symtableInsert :: MemoryCell -> Memory -> Memory
symtableInsert symbol [] = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]

symtableUpdate :: MemoryCell -> Memory -> Memory
symtableUpdate (ID id1 p1, v1) [] = fail $ "Variable " ++ id1 ++ " not found."
symtableUpdate (ID id1 p1, v1) ((ID id2 p2, v2):t) = 
                                if id1 == id2 then (ID id1 p2, v1) : t
                                else (ID id2 p2, v2) : symtableUpdate (ID id1 p1, v1) t

symtableRemove :: MemoryCell -> Memory -> Memory
symtableRemove (ID id1 p1, v1) [] = fail $ "Variable " ++ id1 ++ " not found."
symtableRemove (id1, v1) ((id2, v2):t) = 
                                if id1 == id2 then t
                                else (id2, v2) : symtableRemove (id1, v1) t