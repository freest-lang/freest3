type SyncServer  = *?SyncService
type SyncService = ?Int;Wait

syncServerOnce : Int -> dualof SyncServer -> ()
syncServerOnce limit ch = 
    if limit == 0
    then ()
    else 
        -- create endpoints for syncing
        let (c, s) = new @SyncService () in
        -- send client's endpoint
        let ch = send c ch in
        -- recursive call
        syncServerOnce (limit-1) ch;
        -- sync client
        send 0 s |> close

syncServer : Int -> dualof SyncServer -> ()
syncServer limit ch =
    syncServerOnce limit ch;
    syncServer limit ch

sync : SyncServer -> ()
sync ch =
    -- receive linear sync channel
    let (c, _) = receive ch in
    -- wait for sync
    receiveAndWait @Int c; ()

client : Int -> SyncServer -> ()
client id ch =
    print @Int  (-id);
    sync ch;
    print @Int  id

forkNClients : Int -> SyncServer -> ()
forkNClients i ch =
    if i == 0
    then ()
    else 
        fork (\_:() 1-> client i ch); 
        forkNClients (i-1) ch

nServers : Int
nServers = 20

main : ()
main = 
    let (c, s) = new @SyncServer () in
    forkNClients nServers c;
    syncServer nServers s
