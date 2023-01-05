{-
Unrestricted send.

The type of send is

  ∀a:1T . a *-> ∀b:1S . !a;b 1-> b

Any value (linear or unrestricted) can be sent.

Using eta-conversion one can write a variant of send that accepts only
unrestricted values. The partially evaluated function can then be reused, contrary to a partially evaluated send.
-}

--send : ∀a:1T .     a *-> ∀b:1S .            !a;b 1-> b
unsend : ∀a:*T .     a *-> ∀b:1S .     () *-> !a;b 1-> b
unsend = Λa:*T => λx:a *-> Λb:1S => λ_:() *-> send @a x @b

main : Int
main =
  let (s1, r1) = new @!Int;End () in
  let (s2, r2) = new @!Int;End () in
    
  let sendFive = unsend @Int 5 @End in
  fork (\_:() 1-> sendFive () s1 |> close);
  fork (\_:() 1-> sendFive () s2 |> close);
  {-
  Now let's try with send, rather than unsend:
  let sendFive = send @Int 5 @End in
  fork (\_:() 1-> sendFive s1 |> close);
  fork (\_:() 1-> sendFive s2 |> close);
    Variable or data constructor not in scope: 'sendFive'
  -}
  receiveAndClose @Int r1;
  receiveAndClose @Int r2
  
