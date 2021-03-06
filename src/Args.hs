module Args where

import System.Environment
import System.Console.GetOpt
import Control.Monad
import System.IO

type Exec = (FilePath, [String])

data Target = Record Exec FilePath
            | Replay Exec FilePath
            | Tandem Exec Exec deriving Show
data Mode = RecordMode | ReplayMode | TandemMode deriving (Read, Show)
data Job = Job { jTarget :: Target
               , jTrace  :: Maybe Handle
               , jDump   :: Bool} deriving Show

defaultJob = Job {jTarget = undefined, jTrace = Nothing, jDump = False}

argSpec = [Option ['t'] ["trace"] (ReqArg traceOpt "traceFile") "Optional file to print an execution trace to",
           Option ['d'] ["dump"] (NoArg (\j -> return j {jDump = True})) "Whether to dump cores during execution"]

getJob :: IO Job
getJob = do
   allArgs <- getArgs
   let (opts, (mode : a : b : args'), argsb, _) = getOpt' Permute argSpec allArgs
   let args = argsb ++ args'
   let target = case (read mode) of
                   RecordMode -> Record (a, args) b
                   ReplayMode -> Replay (a, args) b
                   TandemMode -> Tandem (a, args) (b, args)
   foldM (flip ($)) (defaultJob {jTarget = target}) opts

traceOpt path job = do
   fd <- case path of
            "-" -> do hSetBuffering stdout NoBuffering
                      return stdout
            _   -> do fd <- openFile path WriteMode
                      hSetBuffering fd NoBuffering
                      return fd
   return $ job {jTrace = Just fd}
