main : Int
main = 5
main = 6

{- Should not compile

/Users/vv/Desktop/f.hs:3:1: error:
    Multiple declarations of ‘main’
    Declared at: /Users/vv/Desktop/f.hs:2:1
                 /Users/vv/Desktop/f.hs:3:1
  |
3 | main = 6
  | ^^^^

-}
