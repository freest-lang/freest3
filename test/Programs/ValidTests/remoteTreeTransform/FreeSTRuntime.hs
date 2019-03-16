module FreeSTRuntime (_fork, _new, _send, _receive) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Synchronous
import Unsafe.Coerce

_fork e = do
  forkIO e
  return ()

_new = do
  ch1 <- newChan
  ch2 <- newChan
  return ((ch1,ch2), (ch2,ch1))

_send x ch  = do
  writeChan (snd ch) (unsafeCoerce x)
  return ch

_receive ch = do
  a <- readChan (fst ch)
  return (unsafeCoerce a, ch)