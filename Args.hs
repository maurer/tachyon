module Args where

import System.Environment

type Exec = (FilePath, [String])

data Job = Record Exec FilePath
         | Replay Exec FilePath
         | Tandem Exec Exec
data Mode = RecordMode | ReplayMode | TandemMode deriving (Read, Show)

getJob :: IO Job
getJob = do
   (mode : a : b : args) <- getArgs
   case (read mode) of
      RecordMode -> return $ Record (a, args) b
      ReplayMode -> return $ Replay (a, args) b
      TandemMode -> return $ Tandem (a, args) (b, args)
