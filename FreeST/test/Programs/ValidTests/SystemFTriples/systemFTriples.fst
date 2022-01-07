type Triple x y z = ∀k. (x→y→z→k) → k

triple : ∀ a b c . a → b → c → (Triple a b c)
triple = Λ a b c ⇒ λx:a y:b z:c → Λr ⇒ λp:(a->b->c->r) -> p x y z

fst' : ∀ a b c. (Triple a b c) → a
fst' = Λ a b c ⇒ λ p: (Triple a b c) → p [a] (λx:a y:b z:c → x)
-- firstT = Λ a b c ⇒ λ p: (∀r.(a→b→c→r)→r) → p [a] (λx:a y:b z:c → x)

snd' : ∀ a b c . (Triple a b c) → b
snd' = Λ a b c ⇒ λ p: (Triple a b c) → p [b] (λx:a y:b z:c → y)
-- secondT = Λ a b c ⇒ λ p: (∀r.(a→b→c→r)→r) → p [b] (λx:a y:b z:c → y)

trd : ∀ a b c . (Triple a b c) → c
trd = Λ a b c ⇒ λ p: (Triple a b c) → p [c] (λx:a y:b z:c → z)
-- thirdT = Λ a b c ⇒ λ p: (∀r.(a→b→c→r)→r) → p [c] (λx:a y:b z:c → z)

main : Int
main =
  let p = triple [Int,Int,Int] 5 10 8 in
  fst'[Int,Int,Int] p + snd'[Int,Int,Int] p + trd[Int,Int,Int] p