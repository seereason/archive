-- |Limit the number of incremental backups.
module System.Archive.Prune
    ( prune
    , nextVictim
    ) where

import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Function (on)
import Data.List (sortBy, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Time (UTCTime, ZonedTime, NominalDiffTime, parseTime, formatTime, utcToZonedTime, diffUTCTime, zonedTimeToUTC, getCurrentTimeZone, getTimeZone)
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Locale (defaultTimeLocale)
import System.Unix.Process (lazyProcess, exitCodeOnly)

-- |Remove backups until we have a certain number left.
prune :: FilePath -> String -> Int -> IO ()
prune baseDirName prefix keep =
    do paths <- getDirectoryContents baseDirName
       let times = sortBy (flip compare) . mapMaybe (timeFromPath prefix) $ paths
       when -- Always keep two: the oldest and the newest
            (length times > max 2 keep)
            (nextVictim baseDirName prefix >>= \ victim ->
             maybe (return ()) rmrf victim >>
             prune baseDirName prefix keep)

-- |Decide which archives is the least important
nextVictim :: FilePath -> String -> IO (Maybe FilePath)
nextVictim baseDirName prefix =
    do paths <- getDirectoryContents baseDirName
       -- Compute the backup times and sort newest first.
       let times = sortBy (flip compare) . mapMaybe (timeFromPath prefix) $ paths
       case times of
         [] -> return Nothing
         (newest : older) -> do
             let older' = init older
                 ages = map (diffUTCTime newest) older
                 -- For each backup, compute the interval from the
                 -- backup before to the backup after.
                 intervals = map (uncurry diffUTCTime) (zip (newest : older) (tail older))
                 -- The importance of the backup is the quotient of
                 -- the interval just computed and the backup's age.
                 importance :: [(UTCTime, NominalDiffTime)]
                 importance = zip older (map (uncurry (/)) (zip intervals ages))
                 victim :: UTCTime
                 victim = fst . head $ sortBy (compare `on` snd) importance
             let path :: FilePath
                 path = baseDirName </> (prefix ++ formatTime defaultTimeLocale "%F_%T" victim)
             return $ Just path

-- |Parse the date string in a backup directory name.
timeFromPath :: FilePath -> FilePath -> Maybe UTCTime
timeFromPath prefix path =
    stripPrefix prefix path >>=
    parseTime defaultTimeLocale "%F_%T" >>=
    return . zonedTimeToUTC

-- |Remove a directory and its contents.
rmrf :: FilePath -> IO ()
rmrf path =
    lazyProcess "rm" ["-rf", path] Nothing Nothing L.empty >>= return . exitCodeOnly >>= \ code ->
    case code of
      ExitSuccess -> return ()
      code -> error $ "rm -rf " ++ path ++ " -> " ++ show code
