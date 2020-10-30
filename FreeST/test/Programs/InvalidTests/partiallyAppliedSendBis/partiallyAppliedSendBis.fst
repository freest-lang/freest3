f : Int -> !Int -o Skip
f c = send c

main : (Int, Skip)
main =
  let (r, w) = new !Int in
  let _ = fork $ sink $ f 5 r in
  receive w

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
