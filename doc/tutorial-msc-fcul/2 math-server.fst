type MathClient: SL =
  +{ Gz: !Int;?Bool;MathClient
   , Add: !Int;!Int;?Int;MathClient
   , Quit: Skip
   }

aClient : MathClient -> Int
aClient c =
  let (b, c) = receive $ send 7 $ select Gz c in
  printBoolLn b;
  let (n, c) = receive $ send 7 $ send 2 $ select Add c in
  let _ = select Quit c in
  n

theServer : dualof MathClient -> ()
theServer s =
  match s with {
    Gz s ->
      let (n, s) = receive s in
      let s = send (n >= 0) s in
      theServer s,
    Add s ->
      let (n1, s) = receive s in
      let (n2, s) = receive s in
      let s = send (n1 + n2) s in
      theServer s,
    Quit _ -> ()
  }

main : Int
main =
  let (c, s) = new MathClient in
  fork (theServer s);
  aClient c
