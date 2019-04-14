data T = C Int

main : T
main = case C 5 of { C x -> C x }
