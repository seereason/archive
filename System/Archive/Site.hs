{-# LANGUAGE PackageImports #-}
-- |Script to do an incremental backup of an application.  To make it
-- automatic the following steps must be taken, an example can be seen
-- in the creativeprompts package.
--
--   1. Add a section to the cabal file to build the backups binary
--   2. Add a section to debian/rules for the backups package.
--   3. Add a fixup to install the binary into /etc/cron.hourly.
--   4. Add a section to debian/control to create a backups deb.
--   5. Add dependencies on the archive package in debian/control
module System.Archive.Site
       ( BackupTarget(..)
       , backup
       ) where

import Control.Applicative((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Time (getZonedTime, {-getCurrentTime, timeToTimeOfDay, utctDayTime,-} todHour, localTimeOfDay, zonedTimeToLocalTime)
import "Extra" Extra.SSH (sshVerify)
import Network.URI (URIAuth(..))
import System.Archive.Prune (prune)
import System.Archive.UpdateMirror
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStr, hPutStrLn, stderr)

data BackupTarget =
  BackupTarget
  { keep :: Int   -- ^ Maximum number of backups to keep after a cleanup.
  , cleanHour :: Maybe Int
                   -- ^ What time of day (local) should we do cleanups?  (These are disruptive
                   -- to the machine running backup because they involve a lot of rm files.)
  , app :: String  -- ^ Where below top are the site files stored?
  , auth :: URIAuth -- ^ Authorization info for accessing the server
  , localTop :: FilePath
  , remoteTop :: FilePath
  , delay :: Int -- ^ Wait for this many microseconds before beginning
                 -- backup.  This allows backups to be staggered even
                 -- if they are all launched at the same time by a
                 -- cron daemon.
  , nice :: Int  -- ^ Reduce the process priority
  , bwLimit :: Maybe Int -- ^ Tell rsync to limit the bandwidth to this many KBytes/sec
  }

-- main = backup (BackupTarget {app = "seereason-production", user = "upload", host = "seereason.com" }

local :: BackupTarget -> FilePath
local target = localTop target </> app target

-- Should be equivalent to "%Y-%m-%d_%H:%M:%S", but this seems to work
-- better when passed to the prune function and used to parse a date.
format = "%F_%T"

backup :: BackupTarget -> IO ()
backup target =
  do init <- elem "--initialize" <$> getArgs
     case init of
       True -> exitWith ExitSuccess
       False ->
         do threadDelay (delay target)
            hPutStrLn stderr ("Authenticating connection with " ++ pretty (auth target) ++ "...")
            ok <- sshVerify (pretty (auth target)) Nothing
            case ok of
              False ->
                do hPutStr stderr "Authentication failed"
                   exitWith (ExitFailure 1)
              True ->
                do withArgs ["--exclude", "open.lock", app target] (updateMirrorMain (rsyncTargets target))
                   -- utcHour <- (todHour . timeToTimeOfDay . utctDayTime) <$> getCurrentTime
                   localHour <- (todHour . localTimeOfDay . zonedTimeToLocalTime) <$> getZonedTime
                   when (maybe True (== localHour) (cleanHour target))
                        (prune format (local target) (app target ++ "-") (keep target))

pretty auth =
    uriUserInfo auth ++ uriRegName auth ++ uriPort auth

rsyncTargets :: BackupTarget -> [Target]
rsyncTargets target =
    [ RsyncTarget { prettyName = app target
                  , src = [ pretty (auth target) ++ ":" ++ remoteTop target </> app target ]
                  , dest = local target ++ "/"
                  , config = genericConfig (app target) format
                  , options = [Rsync "--progress", Rsync "--stats", Nice (nice target)] ++ maybe [] (\ n -> [Rsync ("--bwlimit=" ++ show n)]) (bwLimit target)
                  }
    ]
