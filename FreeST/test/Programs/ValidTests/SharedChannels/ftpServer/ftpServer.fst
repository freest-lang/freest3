-- 1 _ State

-- |The elements in the state
type File = Int

-- |A bag of files managed by a shared channel.
-- |The elements in the bag are messages in transit.
type State = (*?File, *!File)

-- |Read a file from the state
readFrom : State -> File
readFrom state =
  fst @File @State $ receive $ fst @*?File @*!File state

-- |Write a file to the state
writeTo : File -> State -> ()
writeTo file state =
  send file $ snd @*?File @*!File state; ()

-- 2 _ Types for the FTP server

-- |An FTP server, as seen from the side of an FTP client
-- |Connects clients to the server
type FTP = *?FTPSession
-- | An FTP session seen from the side of the client
type FTPSession : 1S = +
  { Get: ?File ; FTPSession
  , Put: !File ; FTPSession
  , Bye: Skip
  }
-- |An FTP thread channel as seen from the side of the FTP thread
-- |Connects the FTP demon to its threads
type FTPThread = *?(dualof FTPSession)

-- 3 _ The FTP server

-- |Initialise the server: create n FTP threads and launch the demon
init : Int -> dualof FTP -> Diverge
init n pid =
  let (r, w) = new dualof FTPThread in
  let state = new *?File in
  parallel n (\ _:() -> ftpThread state w);
  ftpd pid r

-- |FTP demon: wait for a client, wait for a thread;
-- |pass the client to the thread
ftpd : dualof FTP -> dualof FTPThread -> Diverge
ftpd pid b = 
  send (accept_ @FTPSession pid) b;
  ftpd pid b

-- |An FTP thread: receive a request from the demon;
-- |authenticate the client; pass the thread to the actions loop
ftpThread : State -> FTPThread -> Diverge
ftpThread state b =
  -- TODO: authenticate the client
  actions state (receive_ @dualof FTPSession b) b

-- |A linear interaction with the client;
-- |once done become an FTP thread
actions : State -> dualof FTPSession -> FTPThread 1-> Diverge
actions state s b =
  match s with
    { Get s ->
        let file = readFrom state in
        printIntLn (- file);
        actions state (send file s) b
    , Put s ->
        let (file, s) = receive s in
        printIntLn file;
        writeTo file state;
        actions state s b
    , Bye s -> ftpThread state b
    }

-- Sample clients

-- |Put a file and terminate
putClient : FTP -> File -> Skip
putClient pid file =
  let (c, _) = receive pid in
  select Put c & send file & select Bye

-- |Get a file and terminate
getClient : FTP -> Skip
getClient pid =
  let (c, _) = receive pid in
  let c = select Get c in
  let (file, c) = receive c in
  select Bye c

-- |Put two files and terminate
putClient' : FTP -> File -> File -> Skip
putClient' pid file1 file2 =
  let c = receive_ @FTPSession pid in
  select Put c & send file1 &
  select Put   & send file2 &
  select Bye

-- |Get a file and terminate
putgetClient : FTP -> File -> Skip
putgetClient pid file =
  let (c, _)    = receive pid in
  let c         = select Put c in
  let c         = send file c in
  let c         = select Get c in
  let (file, c) = receive c in
  let c         = select Put c in
  let c         = send file c in
  select Bye c

-- Application

main : Diverge
main =
  let (ftpc, ftps) = new FTP in
  -- A few clients
  fork $ putClient ftpc 27;
  fork $ getClient ftpc;
  fork $ getClient ftpc;
  fork $ putClient' ftpc 93 66;
  fork $ putgetClient ftpc 14;
  fork $ putClient ftpc 59;
  -- A server with three threads
  init 3 ftps
