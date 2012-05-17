{-
This module is a bit hackish from an architecture point of view.
When an ELF program is launched, it is given three basic parameters -
argv, envp, and auxv. Libc consumes the auxv, and usually the rest of
the code only consumes the other two. argv and evnp can and are set
during exec at the moment, so I choose not to worry about them.
Auxv is constant per machine, except for the AT_RANDOM entry.
The purpose of this module is to allow for the AT_RANDOM entry
to be rewritten from one loaded program to another so that their
inputs are actually the same.

TODO
This should eventually be converted to be an AuxV module, and combined
with an args module and an envp module so that we don't rely on exec's
behavior for logically equivalent initial stacks.
-}
module ATRandom (sendATRandom, recvATRandom) where

import Control.Concurrent.STM.BTChan
import System.Trace
import Syscall
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Word
import Foreign.Ptr

buildTPid = P . fromIntegral

wordTrace :: Word64 -> TracePtr a
wordTrace = rawTracePtr . wordPtrToPtr . fromIntegral

atRandSym :: Word64
atRandSym = 25

advanceTo :: (TracePtr Word64) -> Word64 -> Int -> Trace (TracePtr Word64)
advanceTo start target stride = do
  test <- tracePeek start
  if test == target
     then return start
     else advanceTo (start `tracePlusPtr` stride) target stride

getATRand :: Trace (TracePtr Word64)
getATRand = do
  sp <- fmap rsp getRegs
  let argv = wordTrace $ sp + 8
  envp <- fmap (`tracePlusPtr` 8) $ advanceTo argv 0 8
  auxv <- fmap (`tracePlusPtr` 8) $ advanceTo envp 0 8
  atvec <- advanceTo auxv atRandSym 16
  fmap wordTrace $ tracePeek $ atvec `tracePlusPtr` 8

sendATRandom :: BTChan (TPid, Syscall) -> Trace ()
sendATRandom sys = do
  ptr <- getATRand
  a <- tracePeek ptr
  b <- tracePeek (tracePlusPtr ptr 8)
  liftIO $ atomically $ writeBTChan sys (buildTPid 0, ATRand (a, b))
  
recvATRandom :: BTChan (TPid, Syscall) -> Trace ()
recvATRandom sys = do
  (_, ATRand (a, b)) <- liftIO $ atomically $ readBTChan sys
  ptr <- getATRand
  tracePoke ptr a
  tracePoke (tracePlusPtr ptr 8) b
