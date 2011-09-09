module SyscallTable where

import Syscall

import qualified Data.Map as Map

syscallTable :: Map.Map SyscallID SysSig
syscallTable = Map.fromList [(ExecVE, SysSig [String, Strings, Strings]),
                             (Brk, SysSig [RawPtr]), --TODO special (we want this to actually get passed through)
--                             (Open, SysSig [String, Small, Small]),
                             (Open, SysSig [String, Small]),
                             (GetPID, SysSig []),
                             (UName, SysSig [Storage (ConstSize 390)]),
                             (GetArchPrCtl, SysSig [Small, Storage (ConstSize 8)]), -- TODO 8 is the size of an unsigned long, right?
                             (SetArchPrCtl, SysSig [Small, RawPtr]),
                             (ExitGroup, SysSig [Small]),
                             (Access, SysSig [String, Small]),
                             (MMap, SysSig [StorageReccomend (Arg 1), Small, Small, Small, Small, Small]),
                             (SafeMMap, SysSig [StorageReccomend (Arg 1), Small, Small, Small, Small, Small]),
                             (MUnmap, SysSig [RawPtr, Small]),
                             (Stat, SysSig [String, Storage (ConstSize 144)]),
                             (FStat, SysSig [Small, Storage (ConstSize 144)]),
                             (Close, SysSig [Small]),
                             (Read, SysSig [Small, Storage (Arg 2), Small]),
                             (Write, SysSig [Small, InputNull (Arg 2), SmallSize]),
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
                             (StatFS, SysSig [String, Storage (ConstSize 120)]),
                             (IOCtl, SysSig [Small, Small, Storage (ConstSize 60)]), --TODO real signature
                             (FCntl, SysSig [Small, Small]), --TODO real signature
                             (SchedGetParam, SysSig [Small, Storage (ConstSize 4)]),
                             (SchedSetParam, SysSig [Small, Input (ConstSize 4)]),
                             (SchedGetScheduler, SysSig [Small]),
                             (SchedSetScheduler, SysSig [Small, Small, Input (ConstSize 4)]),
                             (ClockGetTime, SysSig [Small, Storage (ConstSize 16)])
                             ]
