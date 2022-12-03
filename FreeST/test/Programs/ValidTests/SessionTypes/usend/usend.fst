{-
Unrestricted send.

The type of send is

  ∀a:1T . a *-> ∀b:1S . !a;b 1-> b

Any value (linear or unrestricted) can be sent.

Using eta-conversion one can write a variant of send that accepts only unrestricted values
-}

--send : ∀a:1T .     a *-> ∀b:1S .            !a;b 1-> b
unsend : ∀a:*T .     a *-> ∀b:1S .     () *-> !a;b 1-> b
unsend = Λa:*T => λx:a *-> Λb:1S => λ_:() *-> send @a x @b

main : Bool
main =
  let (s1, r1) = new !Int;End in
  let (s2, r2) = new !Bool;End in
  fork (\_:() 1-> unsend @Int  5     @End () s1 |> close);
  fork (\_:() 1-> unsend @Bool False @End () s2 |> close);
  -- These work as well for un values can be promoted to lin
  -- fork (\_:() 1-> send @Int  5     @End s1 |> close);
  -- fork (\_:() 1-> send @Bool False @End s2 |> close);
  receiveAndClose @Int  r1;
  receiveAndClose @Bool r2
  
