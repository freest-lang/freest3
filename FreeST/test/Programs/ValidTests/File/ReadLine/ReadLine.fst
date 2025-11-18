import File

readLine : FilePath -> String
readLine filename =
  let inStream = openReadFile filename in
  let (str, inStream') = hGetLine inStream in
  hCloseIn inStream' ;
  str

main : String
main = readLine "test/Programs/ValidTests/File/ReadLine/ReadLine.fst"
