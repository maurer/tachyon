module SyscallTable (syscallTable) where

import Syscall

import qualified Data.Map as Map

int = Small 4
u32 = Small 4
long = slong
slong = Small 8
char = Small 1
void = Small 1
string = Ptr In char (Const 4096) NT
path = string
mode = u32
time = slong
ulong = long
size = Small 8
sMicroSeconds = slong
timeVal = Struct [time,
                  sMicroSeconds]
timeZone = Struct [int, int]
rUsage = flatten $ Struct [timeVal,
                           timeVal,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long,
                           long]
flatten (Struct tys) = Struct $ concatMap (unStruct . flatten) tys
  where unStruct (Struct tys) = tys
        unStruct x = [x]
flatten x = x
fdMask = slong
fdSet = Ptr InOut fdMask (Const 16) UT
clock = ulong
dev = Small 8
ino = Small 8
nlink = Small 8
uid = Small 4
gid = Small 4
off = Small 8
blksize = Small 8
blkcnt = Small 8
stat = Struct [dev,
               ino,
               mode,
               u32, --pad
               nlink,
               uid,
               gid,
               dev,
               off,
               blksize,
               blkcnt,
               time,
               time,
               time]

tms = Struct (replicate 4 clock)

timespec = Struct [time, long]

ptr ioc ty = Ptr ioc ty (Const 1) UT

socklen = Small 4

rawPtr = Ptr Neither void (Const 0) UT

clockId = Small 4

ssize = Small 8

robustListHead = Struct [rawPtr, long, rawPtr] --TODO set it up to walk the list?

iovec m = Struct [Ptr m void (Lookup (Index 1 (Undo Self))) UT,
                  size]

oppose In = Out
oppose Out = In
oppose InOut = InOut

msghdr m = Struct [Ptr m void (Lookup (Index 1 (Undo Self))) UT,
                   socklen,
                   Small 4, -- padding
                   Ptr In (iovec m) (Lookup (Index 4 (Undo Self))) UT,
                   size,
                   Ptr m void (Lookup (Index 6 (Undo Self))) UT,
                   size,
                   int]
nfds = Small 8
short = Small 2
itimer = flatten $ Struct [timeVal, timeVal]
pollfd = Struct [int, short, short]
sigaction = flatten $ Struct [rawPtr,
                              rawPtr,
                              sigset,
                              int,
                              rawPtr]
sigset = Struct $ replicate 16 $ Small 8
argv = Ptr In string (Const 256) NT
rLimit = Struct [rlim, rlim]
rlim = Small 8
utimbuf = Struct [time, time]
uint32 = Small 4
epollData = Small 8
epollEvent = Struct [uint32, epollData]
pid = Small 4
syscallTable :: Map.Map SyscallID SysSig
syscallTable = Map.fromList [(EPollWait, SysSig int [int, Ptr Out epollEvent (Lookup (Arg 2)) UT, int, int]),
                             (NanoSleep, SysSig int [ptr In timespec, ptr Out timespec]),
                             (SetGID, SysSig int [int]),
                             (Wait4, SysSig int [pid, ptr Out int, int, ptr Out rUsage]),
                             (SetGroups, SysSig int [size, Ptr In gid (Lookup (Arg 0)) UT]),
                             (SetUID, SysSig int [uid]),
                             (SetSID, SysSig int []),
                             (ChDir, SysSig int [path]),
                             (GetGID, SysSig gid []),
                             (EPollCreate, SysSig int [int]),
                             (EPollCtl, SysSig int [int, int, int, ptr In epollEvent]),
                             (Listen, SysSig int [int, int]),
                             (Pipe, SysSig int [Ptr Out int (Const 2) UT, int]),
                             (Close, SysSig int [int]),
                             (Access, SysSig int [path, int]),
                             (Open, SysSig int [path, int]),
                             (FAdvise64, SysSig int [int, off, off, int]),
                             (GetEGID, SysSig gid []),
                             (Time, SysSig time [ptr Out time]),
                             (FCntl, SysSig int [int, int]), -- TODO Set up separate sycalls for each command to remove untypedness after this
                             (Read, SysSig int [int, Ptr Out void (Lookup (Arg 2)) UT, size]),
                             (Select, SysSig int [int, fdSet, fdSet, fdSet, Ptr InOut timeVal (Const 1) UT]),
                             (GetTimeOfDay, SysSig int [ptr Out timeVal, ptr Out timeZone]),
                             (GetRUsage, SysSig int [int, ptr Out rUsage]),
                             (GetRLimit, SysSig int [int, ptr Out rLimit]),
                             (Times, SysSig clock [ptr Out tms]),
                             (FStat, SysSig int [int, ptr Out stat]),
                             (MMap, SysSig (Ptr Out void (Lookup (Arg 1)) UT) [rawPtr, size,int, int, int, off]),
                             (Brk, SysSig int [rawPtr]),
                             (IOCtl, SysSig int [int, int]), --TODO Break up into smaller syscalls, this is B.S.
                             (GetEUID, SysSig uid []),
                             (UMask, SysSig mode [mode]),
                             (SocketPair, SysSig int [int, int, int, Ptr Out int (Const 2) UT]),
                             (Dup2, SysSig int [int, int]),
                             (ExecVE, SysSig int [path, argv, argv]), --TODO get userspace loader
                             (GetUID, SysSig int [int]),
                             (UTime, SysSig int [path, Ptr In utimbuf (Const 1) UT]),
                             (Unlink, SysSig int [path]),
                             --TODO HOLY FUCKING GODDAMN SHIT - we can't do this right because it doesn't know where to get the pointer from B|
                             (GetCWD, SysSig rawPtr  [Ptr Out char (Lookup (Arg 1)) NT, size]),
                             --(GetCWD, SysSig (Ptr Out char (Lookup (Arg 1)) NT) [Ptr Out char (Lookup (Arg 1)) NT, size]),
                             (ClockGetTime, SysSig int [clockId, ptr Out timespec]),
                             (Accept, SysSig int [int, Ptr Out void (Lookup (Index 0 (Arg 2))) UT, ptr Out socklen]),
                             (LStat, SysSig int [path, ptr Out stat]),
                             (Socket, SysSig int [int, int, int]),
                             (SafeMMap, SysSig (Ptr Out void (Lookup (Arg 1)) UT) [rawPtr, size,int, int, int, off]),
                             (MProtect, SysSig int [rawPtr, size, int]),
                             (Clone, SysSig int []), -- This is special, as we deal with clone by monitoring the new process/thread
                             (SetRobustList, SysSig long [ptr In robustListHead, size]),
                             (Futex, SysSig int []),
                             (GetITimer, SysSig int [ptr Out itimer]),
                             --(Futex, SysSig int [ptr InOut int, int, int, ptr Out timespec, ptr InOut int, int]), --TODO modalize this syscall
                             (Bind, SysSig int [int, Ptr In void (Lookup (Arg 2)) UT, socklen]),
                             (GetSockName, SysSig int [int, Ptr Out void (Lookup (Index 0 (Arg 2))) UT, ptr Out socklen]),
                             (SendTo, SysSig ssize [int, Ptr In void (Lookup (Arg 2)) UT, size, int, Ptr In void (Lookup (Arg 5)) UT, socklen]),
                             (RecvMsg, SysSig ssize [int, ptr InOut $ msghdr Out, int]),
                             (Connect, SysSig int [int, Ptr In void (Lookup (Arg 2)) UT, socklen]),
                             (MUnmap, SysSig int [rawPtr, size]),
                             (Stat, SysSig int [path, ptr Out stat]),
                             (Poll, SysSig int [Ptr InOut pollfd (Lookup (Arg 1)) UT,  nfds, int]),
                             (RecvFrom, SysSig ssize [int, Ptr Out void (Lookup (Arg 2)) UT, size, int, Ptr Out void (Lookup (Index 0 (Arg 5))) UT, ptr Out socklen]),
                             (MAdvise, SysSig int [rawPtr, size, int]),
                             (ExitSys, SysSig void [int]),
                             (SetSockOpt, SysSig int [int, int, int, Ptr In void (Lookup (Arg 4)) UT, socklen]),
                             (Write, SysSig ssize [int, Ptr In void (Lookup (Arg 2)) UT, size]),
                             (GetSockOpt, SysSig int [int, int, int, Ptr Out void (Lookup (Index 0 (Arg 4))) UT, ptr Out socklen]),
                             (GetPeerName, SysSig int [int, Ptr Out void (Lookup (Index 0 (Arg 2))) UT, ptr Out socklen]),
                             (ExitGroup, SysSig int [int]),
                             (GetPID, SysSig int []),
                             (SetITimer, SysSig int [int, ptr In itimer, ptr Out itimer]),
                             (MkDir, SysSig int [path, mode]),
                             (RTSigAction, SysSig int [int, ptr In sigaction, ptr Out sigaction]),
                             (LSeek, SysSig off [int, off, int]),
                             (GetDEnts, SysSig int [int, Ptr Out void (Lookup (Arg 2)) UT, int]),
                             (RTSigProcMask, SysSig int [int, ptr In sigset, ptr Out sigset]),
                             (TGKill, SysSig int [int, int, int]),
                             (ReadV, SysSig ssize [int, Ptr In (iovec Out) (Lookup (Arg 2)) UT, int]),
                             (WriteV, SysSig ssize [int, Ptr In (iovec In) (Lookup (Arg 2)) UT, int]),
                             (SendFile, SysSig ssize [int, int, ptr InOut off, size]),
                             (Shutdown, SysSig int [int, int]),
                             (SetArchPrCtl, SysSig int [int, rawPtr]) --TODO possibly wrong
                             ]
{-
(GetEUID, SysSig []),
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
-}
