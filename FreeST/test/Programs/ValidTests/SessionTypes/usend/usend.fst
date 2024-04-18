{-
Unrestricted send.

The type of send is

  ∀a . a *-> ∀b . !a;b 1-> b

Any value (linear or unrestricted) can be sent.

Using eta-conversion one can write a variant of send that accepts only
unrestricted values. The partially evaluated function can then be reused, contrary to a partially evaluated send.
-}

unsend : ∀a .     a *-> ∀b .     () *-> !a;b 1-> b
unsend = Λa => λx:a *-> Λb => λ_:() *-> send @a x @b

main : Int
main =
  let (s1, r1) = new @(!Int;Close) () in
  let (s2, r2) = new @(!Int;Close) () in
    
  let sendFive = unsend @Int 5 @Close in
  fork (\_:() 1-> sendFive () s1 |> close);
  fork (\_:() 1-> sendFive () s2 |> close);

-- Now let's try with send, rather than unsend:
  -- let sendFive = send @Int 5 @Close in
  -- fork (\_:() 1-> sendFive s1 |> close);
  -- fork (\_:() 1-> sendFive s2 |> close);
-- Variable or data constructor not in scope: 'sendFive'

  receiveAndWait @Int r1 + receiveAndWait @Int r2
