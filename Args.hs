module Args where

import System.Environment

type Exec = (FilePath, [String])

data Job = Record Exec FilePath
         | Replay Exec FilePath

data Mode = RecordMode | ReplayMode deriving (Read, Show)

getJob :: IO Job
getJob = do
   (mode : a : b : args) <- getArgs
   case (read mode) of
      RecordMode -> return $ Record (a, args) b
      ReplayMode -> return $ Replay (a, args) b
