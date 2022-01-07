------------------------------------------------------------
-- Interacting with session types (a simple protocol)
------------------------------------------------------------

type AbsContinuatuion a:SL = !Int;a

send1 : AbsContinuatuion !Bool -> Skip
send1 c = send 5 c & send True

recv1 : dualof (AbsContinuatuion !Bool) -> (Int, Bool)
recv1 c =
  let (i, c) = receive c in
  let (b, _) = receive c in (i, b)
  
send2 : AbsContinuatuion !Char -> Skip
send2 c = send 5 c & send 'a'

recv2 : dualof (AbsContinuatuion !Char) -> (Int, Char)
recv2 c =
  let (i, c) = receive c in
  let (b, _) = receive c in (i, b)


main : Int
main =
 let (s1, r1) = new AbsContinuatuion !Bool in
 let (s2, r2) = new AbsContinuatuion !Char in
 send1 s1 ; send2 s2 ;
 let (i1, b) = recv1 r1 in
 let (i2, c) = recv2 r2 in
 printString "i1: "; printIntLn i1 ;
 printString "i2: "; printIntLn i2 ;
 printString "b: "; printBoolLn b  ;
 printString "c: "; printCharLn c  ;
 -1
