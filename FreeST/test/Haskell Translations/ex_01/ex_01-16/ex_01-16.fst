-- I exercise 16

-- a
data CharList = CEnd | CharList Char CharList
-- CharList 'a' (CharList 'b' (CharList 'c' CEnd))

-- b
data TripleCharTuple = TCTuple Char Char Char
-- TCTuple 'a' 'b' 'c'

-- c
-- inexistent

-- d
data BoolList = BEnd | BoolList Bool BoolList
-- BoolList True (BoolList False BEnd)

-- e
data CharCharList = CCEnd | CharCharList CharList CharCharList
-- CharCharList (CharList 'a' (CharList 'b' (CEnd))) (CharCharList (CharList 'c' (CharList 'd' (CEnd))) CCEnd)

-- f
data CharBoolTuple = CBTuple Char Bool
data CharBoolTupleList = CBTEnd | CBTList CharBoolTuple CharBoolTupleList
-- CBTList (CBTuple 'a' False) (CBTList (CBTuple 'b' True) CBTEnd)

-- g
data CharBoolFunction = F (Char -> Bool)        -- similar to an interface
-- BoolList (isDigit 'a') (BoolList (isLower 'b') (BoolList (isUpper 'c') BEnd))

-- h
data CharListBollListTuple = Tuple CharList BoolList
-- Tuple (CharList 'a' (CharList 'b' CEnd)) (BoolList False (BoolList True BEnd))

-- i
data CharBoolFunctionList = FEnd | FList CharBoolFunction CharBoolFunctionList
-- FList (F isDigit) (FList (F isLower) (FList (F isUpper) FEnd))

main : Bool
main =
    let _ = CharList 'a' (CharList 'b' (CharList 'c' CEnd))                                                             in -- a
    let _ = TCTuple 'a' 'b' 'c'                                                                                         in -- b
    let _ = BoolList True (BoolList False BEnd)                                                                         in -- d
    let _ = CharCharList (CharList 'a' (CharList 'b' (CEnd))) (CharCharList (CharList 'c' (CharList 'd' (CEnd))) CCEnd) in -- e
    let _ = CBTList (CBTuple 'a' False) (CBTList (CBTuple 'b' True) CBTEnd)                                             in -- f
    let _ = BoolList (isDigit 'a') (BoolList (isLower 'b') (BoolList (isUpper 'c') BEnd))                               in -- g
    let _ = Tuple (CharList 'a' (CharList 'b' CEnd)) (BoolList False (BoolList True BEnd))                              in -- h
    let _ = FList (F isDigit) (FList (F isLower) (FList (F isUpper) FEnd))                                              in -- i
    True

-- "pure fabrication"
isDigit : Char -> Bool
isDigit _ = True
isLower : Char -> Bool
isLower _ = False
isUpper : Char -> Bool
isUpper _ = False
