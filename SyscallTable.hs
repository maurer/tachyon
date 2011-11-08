module SyscallTable where

import Syscall

import qualified Data.Map as Map

syscallTable :: Map.Map SyscallID SysSig
syscallTable = Map.fromList [(GetEUID, SysSig []),
-- Clone is special, we handle an event for its result
                             (Clone, SysSig [Small, Small]),
                             (FTruncate, SysSig [Small, Small]),
                             (Dup2, SysSig [Small, Small]),
                             (UMask, SysSig [Small]),
                             (GetUID, SysSig []),
                             (SetUID, SysSig [Small]),
                             (SetGID, SysSig [Small]),
                             (SetSockOpt, SysSig []), --TODO
                             (Times, SysSig[Storage (ConstSize 32)]),
                             (Pipe, SysSig[InOut (ConstSize 8)]), --TODO not passthrough, but some kind of emulation...
 --TODO do select right
                       --      (Select, SysSig[Small, Input (ConstSize 80), Input (ConstSize 80), Input (ConstSize 80), Input (ConstSize 10)]), --TODO probably also a passthrough
                             (Select, SysSig[]),
                             (GetEGID, SysSig []),
                             (SetRLimit, SysSig [Small, Input (ConstSize 10)]),
                             (ExecVE, SysSig [String, Strings, Strings]),
                             (Bind, SysSig [Small, Input (Arg 2), Small]),
                             (RecvMsg, SysSig [Small, Storage (ConstSize 56), Small]),
--TODO check poll more carefully
                             (Poll, SysSig [Input (Count 56 1), Small, RawPtr, RawPtr]), 
                             (RecvFrom, SysSig [Small, Storage (Arg 2), Small, Small, Storage (ArgMan 5), Small]),
                             (GetSockName, SysSig [Small, Storage (ArgMan 2), Small]),
                             (SendTo, SysSig [Small, Input (Arg 2), Small, Small]),
                             (Brk, SysSig [RawPtr]), --TODO special (we want this to actually get passed through)
--                             (Open, SysSig [String, Small, Small]),
                             (Open, SysSig [String, Small]),
                             (SetResUID, SysSig [Small, Small, Small]),
                             (SetResGID, SysSig [Small, Small, Small]),
                             (GetPID, SysSig []),
                             (UName, SysSig [Storage (ConstSize 390)]),
                             (GetArchPrCtl, SysSig [Small, Storage (ConstSize 8)]), -- TODO 8 is the size of an unsigned long, right?
                             (Time, SysSig [MaybeStorage (ConstSize 8)]),
                             (LSeek, SysSig [Small, Small, Small]),
                             (SetArchPrCtl, SysSig [Small, RawPtr]),
                             (ExitGroup, SysSig [Small]),
                             (Access, SysSig [String, Small]),
                             (GetPeerName, SysSig [Small, Storage (ArgMan 2), Small]),
                             (Socket, SysSig [Small, Small, Small]),
                             (Connect, SysSig [Small, Storage (Arg 2), Small]),
                             (GetCWD, SysSig [Storage (Arg 1), Small]),
                             (LStat, SysSig [InputNull (ConstSize 1024), Small]),
--TODO use PATH_MAX?
                             (GetSockOpt, SysSig [Small, Small, Small, Storage (ArgMan 4), Small]),
                             (CapGet, SysSig [Small, Small]),
                             (MMap, SysSig [StorageReccomend (Arg 1), Small, Small, Small, Small, Small]),
                             --(MMap, SysSig[]),
                             (MAdvise, SysSig []), --TODO
                             (ExitSys, SysSig[Small]),
                             (SafeMMap, SysSig [StorageReccomend (Arg 1), Small, Small, Small, Small, Small]),
                             (MUnmap, SysSig [RawPtr, Small]),
                             (Stat, SysSig [String, Storage (ConstSize 144)]),
                             (WriteV, SysSig []),
                             (FStat, SysSig [Small, Storage (ConstSize 144)]),
                             (Close, SysSig [Small]),
                             (Read, SysSig [Small, Storage (Arg 2), Small]),
                             (Write, SysSig [Small, Input (Arg 2), SmallSize]),
                             (MProtect, SysSig [RawPtr, Small, Small]),
                             (SetTIDAddr, SysSig [RawPtr]),
                             (GetDEnts, SysSig [Small, Storage (Arg 2), Small]),
                             (SetRobustList, SysSig [Storage (Arg 1), Small]),
                             (GetTimeOfDay, SysSig [Storage (ConstSize 16), RawPtr]),
                             (Futex, SysSig []), --TODO :(
                             (RTSigAction, SysSig []),
--                             (RTSigAction, SysSig [Small, Input (Arg 3), Storage (Arg 3), Small]), --TODO better equivalency
                             (RTSigProcMask, SysSig []), --TODO real signature
                             (GetRLimit, SysSig []),--TODO real signature
                             (GetRUsage, SysSig []), --TODO real signature
                             (Unlink, SysSig [InputNull (ConstSize 1024), Small]),
                             (StatFS, SysSig [String, Storage (ConstSize 120)]),
                             (IOCtl, SysSig [Small, Small, Storage (ConstSize 60)]), --TODO real signature
                             (FCntl, SysSig [Small, Small]), --TODO real signature
                             (SchedGetParam, SysSig [Small, Storage (ConstSize 4)]),
                             (SchedSetParam, SysSig [Small, Input (ConstSize 4)]),
                             (SchedGetScheduler, SysSig [Small]),
                             (SchedSetScheduler, SysSig [Small, Small, Input (ConstSize 4)]),
                             (ClockGetTime, SysSig [Small, Storage (ConstSize 16)])
                             ]
