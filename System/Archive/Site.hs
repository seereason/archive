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
import Extra.SSH (sshVerify)
import System.Archive.Prune (prune)
import System.Archive.UpdateMirror
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStr, hPutStrLn, stderr)

data BackupTarget =
  BackupTarget
  { app :: String  -- ^ Where in /srv are the site files stored?
  , user :: String -- ^ What user should we use to log into the server?
  , host :: String -- ^ What host is the server on?
  , keep :: Int    -- ^ Maximum number of backups to keep.
  }

-- main = backup (BackupTarget {app = "seereason-production", user = "upload", host = "seereason.com" }

local :: BackupTarget -> FilePath
local target = "/srv/backups" </> app target

-- Should be equivalent to "%Y-%m-%d_%H:%M:%S", but this seems to work
-- better when passed to the prune function and used to parse a date.
format = "%F_%T"

backup :: BackupTarget -> IO ()
backup target =
  do init <- elem "--initialize" <$> getArgs
     case init of
       True -> exitWith ExitSuccess
       False ->
         do hPutStrLn stderr ("Authenticating connection with " ++ user target ++ "@" ++ host target ++ "...")
            ok <- sshVerify (user target ++ "@" ++ host target) Nothing
            case ok of
              False ->
                do hPutStr stderr "Authentication failed"
                   exitWith (ExitFailure 1)
              True ->
                do withArgs ["--exclude", "open.lock", app target] (updateMirrorMain (rsyncTargets target))
                   prune format (local target) (app target ++ "-") (keep target)

rsyncTargets :: BackupTarget -> [Target]
rsyncTargets target =
    [ RsyncTarget { prettyName = app target
                  , src = [ user target ++ "@" ++ host target ++ ":/srv/" ++ app target ]
                  , dest = local target ++ "/"
                  , config = genericConfig (app target) format
                  , options = [Rsync "--progress", Rsync "--stats"]
                  }
    ]
