type SyncServer  = *?SyncService
type SyncService = ?Int

syncServer : Int -> dualof SyncServer -> ()
syncServer limit ch = 
    if limit == 0
    then ()
    else 
        -- create endpoints for syncing
        let (c, s) = new SyncService in
        -- send client's endpoint
        let ch = send c ch in
        -- recursive call
        syncServer (limit-1) ch;
        -- sync client
        let _ = send 0 s in
        ()


sync : SyncServer -> ()
sync ch =
    -- receive linear sync channel
    let (c, _) = receive ch in
    -- wait for sync
    let _ = receive c in
    ()

thread : Int -> SyncServer -> ()
thread id ch =
    wait $ id * 100;
    sync ch;
    putIntLn id

forkNThreads : Int -> SyncServer -> ()
forkNThreads i ch =
    if i == 0
    then ()
    else 
        fork thread ch; 
        forkNThreads (i-1) ch

main : ()
main = 
    let (c, s) = new SyncServer in
    forkNThreads 20 c;
    syncServer 20 s