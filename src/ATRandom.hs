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
module ATRandom where

import Control.Concurrent.STM.BTChan
import System.Trace
import Syscall

type ATRand = ()

sendATRandom :: BTChan (TPid, Syscall) -> Trace ()
sendATRandom = undefined
recvATRandom :: BTChan (TPid, Syscall) -> Trace ()
recvATRandom = undefined
