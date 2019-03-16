module FreeSTRuntime (_fork, _new, _send, _receive) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Synchronous (newChan, writeChan, readChan)
import Unsafe.Coerce (unsafeCoerce)

_fork e = forkIO e >> return ()

_new = newChan >>= \ch -> return (ch, ch)

_send x ch  = writeChan ch (unsafeCoerce x) >> return ch

_receive ch = readChan ch >>= \a -> return (unsafeCoerce a, ch)
