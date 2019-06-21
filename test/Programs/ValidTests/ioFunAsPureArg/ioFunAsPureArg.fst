main : Bool
main =
  let s, r = new !Int in
  let _ = fork (sender s 5) in
  if (div (f r) 2) == 5 then
    True
  else
    False

f : ?Int -> Int
f c = let x, c = receive c in x

sender : !Int -> Int -> Skip
sender c i = send c (i * 2)
-- let _ = send c (i * 2) in ()



-- problem with code gen; compare with generated code

-- _main = _new >>= \(s, r) -> _fork (((sender s) 5) >> return ()) >>= \_ ->
--   ((io_p_io_(==) ((io_p_io_div (f r)) 2)) 5) >>= \_x2 -> if _x2 then return True else return False

-- f = (\c -> (_receive c) >>= \(x, c) -> return x)

-- sender = (\c -> (\i -> ((_send c) (((*) i) 2))))


-- io_p_io_div = \_x0 -> \_x1 -> _x0 >>= \_x3 -> return (div _x3 _x1)

-- io_p_io_(==) = \_x0 -> \_x1 -> _x0 >>= \_x3 -> return ((==) _x3 _x1)

-- main = _main >>= \res -> putStrLn (show (res :: Bool))
