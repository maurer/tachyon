module Syscall where

import System.Trace
import Data.Int
import Util
import Data.Binary
import Data.Word
import qualified Data.ByteString as BS

data SysReq = SysReq SyscallID [(Lookup, Datum)] deriving (Show, Eq)
data SysRes = SysRes Word64 [(Lookup, Datum)] deriving Show

data Syscall = Syscall SysReq SysRes
             | ATRand (Word64, Word64) deriving Show -- TODO ATRand doesn't belong here, this is a mistake, but I want to avoid some stuff for now.

data SysSig = SysSig Type [Type] deriving Show

data Datum = SmallDatum Word64 | Buf BS.ByteString deriving (Show, Eq)
data IOC = Neither | In | Out | InOut deriving Show
data NT = NT | UT deriving (Show, Eq)
data Lookup = Arg Int | Index Int Lookup | Self | Undo Lookup deriving (Show, Eq)
data Bound = Const Int | Mult Bound Int | Lookup Lookup deriving Show
data Type = Small Int
          | Struct [Type]
          | Ptr IOC Type Bound NT deriving Show

data SyscallID = Dup2
               | SocketPair
               | MAdvise
               | UMask
               | SetResUID
               | SetResGID
               | Read
               | Write
               | Open
               | Close
               | Stat
               | FStat
               | LStat
               | Poll
               | LSeek
               | MMap
               | SafeMMap
               | MProtect
               | RTSigProcMask
               | ExitGroup
               | RTSigAction
               | Brk
               | Access
               | ExecVE
               | GetPID
               | MUnmap
               | SetArchPrCtl
               | GetArchPrCtl
               | SetTIDAddr
               | Futex
               | SetRobustList
               | GetRLimit
               | GetRUsage
               | SetRLimit
               | Clone
               | Unlink
               | StatFS
               | IOCtl
               | UTime
               | SetITimer
               | GetITimer
               | SchedGetParam
               | SchedSetParam
               | FCntl
               | GetDEnts
               | UName
               | GetTimeOfDay
               | SchedGetScheduler
               | SchedSetScheduler
               | ClockGetTime
               | Time
               | GetPeerName
               | Socket
               | SetSockOpt
               | Connect
               | GetCWD
               | GetSockOpt
               | CapGet
               | Bind
               | GetSockName
               | SendTo
               | RecvMsg
               | RecvFrom
               | GetEUID
               | ExitSys
               | Select
               | FTruncate
               | EPollCreate
               | EPollCtl
               | GetUID
               | SetUID
               | Pipe
               | SetGID
               | GetEGID
               | Times
               | WriteV
               | MkDir
               | EPollWait
               | TGKill
               | Listen
               | SetSID
               | ChDir
               | GetGID
               | SetGroups
               | Wait4
               | Accept
               | ReadV
               | NanoSleep
               | SendFile
               | Shutdown
               | FAdvise64
               | ReadLink
                deriving (Ord, Show, Eq, Read, Enum)

syscallReg = orig_rax

syscallID :: Regs -> SyscallID
syscallID regs = case syscallReg regs of
   0   -> Read
   1   -> Write
   2   -> Open
   3   -> Close
   4   -> Stat
   5   -> FStat
   6   -> LStat
   7   -> Poll
   8   -> LSeek
   9   -> case (bitCast $ r8 regs) :: Int32 of
             -1 -> SafeMMap
             _ -> MMap
   10  -> MProtect
   11  -> MUnmap
   12  -> Brk
   13  -> RTSigAction
   14  -> RTSigProcMask
   16  -> IOCtl
   19  -> ReadV
   20  -> WriteV
   21  -> Access
   22  -> Pipe
   23  -> Select
   28  -> MAdvise
   33  -> Dup2
   35  -> NanoSleep
   36  -> GetITimer
   38  -> SetITimer
   39  -> GetPID
   40  -> SendFile
   41  -> Socket
   42  -> Connect
   43  -> Accept
   44  -> SendTo
   45  -> RecvFrom
   47  -> RecvMsg
   48  -> Shutdown
   49  -> Bind
   50  -> Listen
   51  -> GetSockName
   52  -> GetPeerName
   53  -> SocketPair
   54  -> SetSockOpt
   55  -> GetSockOpt
   56  -> Clone
   59  -> ExecVE
   60  -> ExitSys
   61  -> Wait4
   63  -> UName
   72  -> FCntl
   77  -> FTruncate
   78  -> GetDEnts
   79  -> GetCWD
   80  -> ChDir
   87  -> Unlink
   83  -> MkDir
   89  -> ReadLink
   95  -> UMask
   96  -> GetTimeOfDay
   97  -> GetRLimit
   98  -> GetRUsage
   100 -> Times
   102 -> GetUID
   104 -> GetGID
   105 -> SetUID
   106 -> SetGID
   107 -> GetEUID
   108 -> GetEGID
   112 -> SetSID
   116 -> SetGroups
   117 -> SetResUID
   119 -> SetResGID
   125 -> CapGet
   132 -> UTime
   137 -> StatFS
   142 -> SchedSetParam
   143 -> SchedGetParam
   144 -> SchedSetScheduler
   145 -> SchedGetScheduler
   158 -> case rdi regs of
             0x1001 -> SetArchPrCtl
             0x1002 -> SetArchPrCtl
             0x1003 -> GetArchPrCtl
             0x1004 -> GetArchPrCtl
   160 -> SetRLimit
   201 -> Time
   202 -> Futex
   213 -> EPollCreate
   218 -> SetTIDAddr
   221 -> FAdvise64
   228 -> ClockGetTime
   231 -> ExitGroup
   232 -> EPollWait
   233 -> EPollCtl
   234 -> TGKill
   273 -> SetRobustList
   n   -> error $ "Unknown syscall: " ++ (show n)

instance Binary SyscallID where
   put x = put $ fromEnum x
   get = fmap toEnum get

instance Binary Syscall where
   put (Syscall p o) = do put (0 :: Word8)
                          put p
                          put o
   put (ATRand (a, b)) = do put (1 :: Word8)
                            put a
                            put b
   get = do m <- get
            case (m :: Word8) of
              0 -> do p <- get
                      o <- get
                      return $ Syscall p o
              1 -> do a <- get
                      b <- get
                      return $ ATRand (a, b)

instance Binary SysReq where
  put (SysReq n x) = do put n
                        put x
  get = do n <- get
           x <- get
           return $ SysReq n x

instance Binary SysRes where
   put (SysRes n x) = do
      put n
      put x
   get = do
      n <- get
      x <- get
      return (SysRes n x)

encodeThresh = 5
data NullHelp = Rep Int Word8 | Lit BS.ByteString deriving (Read, Show, Eq)
nullEncode :: BS.ByteString -> NullString
nullEncode ns = reverse $ fst $ foldl fuse ([], []) $ map help $ BS.group ns
      where help x | BS.length x >= encodeThresh = Rep (BS.length x) (BS.head x)
                   | otherwise = Lit x
            fuse (xs, fusing) (Lit toFuse) = (xs, (toFuse : fusing))
            fuse (xs, []) rep = (rep : xs, [])
            fuse (xs, fusing) rep = (rep : (Lit $ BS.concat $ reverse fusing) : xs, [])
nullUnbox :: NullString -> BS.ByteString
nullUnbox x = BS.concat $ map unHelp x
      where unHelp (Rep n v) = BS.replicate n v
            unHelp (Lit s) = s

instance Binary Datum where
   put (Buf x) = do put (0 :: Word8)
                    put x
   put (SmallDatum n) = do put (1 :: Word8)
                           put n
   get = do (c :: Word8) <- get
            case c of
               0 -> do x <- get
                       return $ Buf x
               1 -> do x <- get
                       return $ SmallDatum x


instance Binary Lookup where
  put (Arg n) = do put (0 :: Word8)
                   put n
  put (Index n l) = do put (1 :: Word8)
                       put n
                       put l
  put Self        = put (2 :: Word8)
  put (Undo l)    = do put (3 :: Word8)
                       put l
  get = do (c :: Word8) <- get
           case c of
             0 -> do n <- get
                     return $ Arg n
             1 -> do n <- get
                     l <- get
                     return $ Index n l
             2 -> return Self
             3 -> fmap Undo get
type NullString = [NullHelp]
