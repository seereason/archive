module Archive 
    ( Option(..)
    , UpdateResult(..)
    , Config(..)
    , archive
    , configLaws
    , genericConfig
    ) where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Linspire.Unix.FilePath
import Linspire.Unix.Files
import System.Directory
import System.IO
import System.IO.Error
import System.Locale
import System.Posix.Files
import System.Process
import System.Exit
import Test.HUnit.Base
import Text.Regex.Posix


{-

To rsync we need:

 1. the latest 'complete' version
 2. the latest incomplete ?

If an incomplete archive exists we can 'ignore' it. One of two things will happen:

 1. it will be an incomplete archive from another day, and it will just sit there being ignored
 2. it will be an incomplete archive from today -- in which case we will resume the aborted transfer

If we resume the aborted transfer, then we might also want to add
--delete to remove things that have disappeared from the original in
the meantime.

If the incomplete is older than the most recent up to date, then it is
useless. If it is newer, then we could resume it. We can also make the
resume optional. If we make resume optional then we could end up with
several failed downloads. Also, their might be a conflict with the
date. Unless, we delete failed downloads that are not resumed.

So the option for resume should be, if failed downloads are found,
(resume|delete). Ignore is a problem if it is the same date, unless we
move it out of the way. Perhaps the currently active one should be
.inprogress, and renamed to incomplete if failure occurs and we do not
resume.

of course, we could then get the date.incomplete and date.inprogress,
where date is the same, and then the rename would fail, unless detects
that case.

There are three types of directories:

- current
- inprogress
- incomplete

-- * Requirements

We wish to make a bunch of snapshot directories from a
source. snapshots should be hardlinked together to save disk space.

It should be possibly to distinguish between a directory that is
complete vs incomplete. There could also be unreleated directories. It
would be nice to be able to distinguish snapshot from non-snapshot
directories.

The naming of the directories should be configurable.

It should be possible to recover from an aborted download.

A directory only has two states:

 + incomplete
 + complete

Should be possible to update the latest snapshot instead of creating a
new snapshot.

It would be nice to avoid using the --delete flag on rsync. However,
that means we have to delete incomplete downloads some other way
anyway. So, it might be better to use the --delete flag, and instead
have the rule that there should only be one inprogress directory at a
time. If the inprogress directory is not timestamped, that makes it
easier to rename it at the end, etc. However, the timestamp will then
the completion time, not the start time of the download. Unless we
just save the time stamp in the code.

The only useful thing you can do with an incomplete download is to
resume it. The primary advantage to not recovering it is you do not
need to use the --delete flag. However, if you do not resume it, then
you want to delete it anyway -- so it really just comes down what form
of deletion do you feel is safest.

Storing information in the file name means parsing of filenames, which
seems error prone. But, perhaps that is unavoidable.


-- * How

 using rync with --link-dest to create hardlinks to previous versions. 

-}

data Report = Found (Maybe FilePath) [FilePath] [FilePath]
            deriving (Show, Read, Eq)

newtype Date = Date String deriving (Show, Read, Ord, Eq)

-- |These represent the command line options to this program.
data Option
    = {- DryRun		-- ^Print the commands to execute, done run them
    | Unlink		-- ^Unimplemented
    | Current		-- ^Unimplemented
    | Prune String	-- ^Unimplemented 
    | -}  Rsync String	-- ^Pass one or more additional arguments to the rsync sub-process
    | NoUpdateSymlink
    deriving (Eq, Show)

-- |Did the update result in any changes
data UpdateResult 
    = NoChanges -- ^ rsync did not detect any changes
    | Changes   -- ^ rsync found some changes
      deriving (Show, Read, Eq)

-- |configuration for directory naming
--
-- You can configure the 
-- see also: 'configLaws'
data Config
    = Config { mkName :: ZonedTime -> FilePath		-- ^ generate a filepath for the completed snapshot directory
             , date :: FilePath -> Date			-- ^ return the date portion of a filepath
             , isComplete :: FilePath -> Bool		-- ^ predicate which tests if a filepath represents a completed snapshot
             , mkInProgress :: ZonedTime -> FilePath	-- ^ generate a filepath for the snapshot directory to be called while the download is in progress
             , isInProgress :: FilePath -> Bool		-- ^ predicate which tests if a file represents an inprogress snapshot
             }

-- * Example Configuration


-- |create a config using the supplied basename and time format
-- see also: 'configLaws'
genericConfig :: FilePath -> String -> Config
genericConfig baseDirName formatString =
    Config { mkName = mkName'
           , date = Date . reverse . takeWhile (/= '.') . reverse
           , isComplete = \fp -> not $ any (flip isPrefixOf fp) ["outofdate.", "inprogress.", "incomplete."]
           , mkInProgress = \zonedtime -> "inprogress." ++ mkName' zonedtime
           , isInProgress = isPrefixOf "inprogress."
           }
    where
      mkName' zonedtime =
          baseDirName ++ "-" ++ formatTime defaultTimeLocale formatString zonedtime

-- |You can (and should) test that all the laws hold for your config
-- by using the 'configLaws' function. For example,
--
-- >	Test.HUnit.Text.runTestTT (configLaws (genericConfig "snapshot" "%Y-%m-%d"))
--
-- The user supplied configuration
-- must uphold the following laws:
--
-- 1. completed directories should be listed chronological when sorted
-- lexigraphically
--
-- >	forall date0 date1. (date0 < date1) => (mkName date0) < (mkName date1)
--
-- 2. the date function should return the same date if 'mkName' and
--    'mkInProgress' are applied to the same date.
--
-- >	date . mkName == date . mkInProgress
--
-- 3. 'mkName' should generate a filepath which tests as 'True' by 'isComplete'.
--
-- >	isComplete . mkName = const True
--
-- 4. 'mkInProgress' should generate a filepath which tests as 'True' by 'isInProgress'
--
-- >	isInProgres . mkInProgress = const True
--
-- 5. The filepaths generated by 'mkName' and 'mkInProgress' should
--    generate names which can be differentiated by 'isComplete' and
--    'isInProgress'.
-- 
-- >	(\fp -> (isInProgress fp) == True => (isComplete   fp) == False) &&
-- >	(\fp -> (isCompletefp fp) == True => (isInProgress fp) == False))
configLaws :: Config -> Test
configLaws config =
    let zt0 =  utcToZonedTime utc (posixSecondsToUTCTime  (realToFrac (0 :: Integer)))
        zt1 =  utcToZonedTime utc (posixSecondsToUTCTime  (realToFrac (100000000 :: Integer)))
        zt0s = formatTime defaultTimeLocale rfc822DateFormat zt0
        zt1s = formatTime defaultTimeLocale rfc822DateFormat zt1
    in
      test [ assertBool ("mkName '" ++ zt0s ++ "' < mkName '" ++ zt1s ++"'")  ((mkName config zt0) < (mkName config zt1))
           -- , assertBool ("date (mkName '" ++ zt0s ++"') == date (mkInProgress '" ++ zt0s ++"')") ((date config (mkName config zt0)) == (date config (mkInProgress config zt0)))
           , assertBool ("date . mkName == date . mkInProgress") ((date config (mkName config zt0)) == (date config (mkInProgress config zt0)))
           , assertBool ("isComplete . mkName == const True") (isComplete config (mkName config zt0) == True)
           , assertBool ("isInProgress . mkInProgress == const True") (isInProgress config (mkInProgress config zt0) == True)
           , assertBool ("(isInProgress == True) => (isComplete == False) && (isComplete == True) => (isInProgress False)")
                            (let ip = (mkInProgress config zt0)
                                 cp = mkName config zt0
                             in
                               (not (isComplete config ip) && not (isInProgress config cp)))
           ]

archive :: Config -> [Option] -> FilePath -> FilePath -> IO (Maybe UpdateResult)
archive config options src backupdir = 
    do dirs <- getSnapshotDirectories (const True) backupdir
       let report = latest config dirs
       update config options src backupdir report

update :: Config -> [Option] -> FilePath -> FilePath -> Report -> IO (Maybe UpdateResult)
update config options src snapshotDir (Found mPrev inprogress _obsolete) =
    do ct <- getZonedTime
       if mPrev == (Just (mkName config ct))
        then error "update in place" -- updateInPlace (mkName ct)
        else do let inProgressFP = snapshotDir +/+ (mkInProgress config ct)
                    completedFP  = snapshotDir +/+ (mkName config ct)
                (ec, mChanges) <- rsync options (map (snapshotDir +/+) (inprogress ++ maybeToList mPrev)) src inProgressFP
                case ec of
                  (ExitFailure n) ->
                      do hPutStrLn stderr ("rsync failed with exit code " ++ show n)
                         exitWith (ExitFailure n)
                  ExitSuccess ->
                      do renameDirectory inProgressFP completedFP `catch`
                           (\e -> error $ "update: failed to rename: " ++ inProgressFP ++ " to " ++ completedFP ++ "\n" ++ show e)
                         unless (NoUpdateSymlink `elem` options) (forceSymbolicLink completedFP "current")
                         return mChanges

rsync :: [Option] -> [FilePath] -> FilePath -> FilePath -> IO (ExitCode, Maybe UpdateResult)
rsync options linkDests src dest =
    do absLinkDests <- mapM realpath linkDests
       let cmd = "rsync"
           args =
            ((map ("--link-dest=" ++) absLinkDests) ++
             (mapMaybe rsyncOption options) ++
             ["-a" -- implies: lptgoD
             , "-HxS"
             , "--partial"
             , "--delete" -- the deletes are really only needed for update-in-place
             , "--delete-excluded"
             , "--stats"
             , src
             , dest
             ]
            )
       hPutStrLn stderr ("> " ++ unwords (cmd : args))
       hPutStrLn stderr ("  Updating from " ++ src ++ " ...")
       (hin, hout, herr, ph) <- runInteractiveProcess cmd args Nothing Nothing
       hClose hin
       out <- hGetContents hout
       forkIO $ hPutStrLn stdout out
       forkIO $ hGetContents herr >>= hPutStrLn stderr
       ec <- waitForProcess ph
       case (out =~ "Total transferred file size: ([0-9]*) bytes") :: (String, String, String, [String]) of
           (_,_,_,[s]) -> return (ec, Just (if s == "0" then NoChanges else Changes ))
           _ -> return (ec, Nothing)

    where
      rsyncOption (Rsync x) = Just x
      rsyncOption _ = Nothing

-- return the latest *complete* archive, latest *inprogress* archives which are
-- newer than the latest *complete*, and a list of any other
-- *inprogress*

latest :: Config
       -> [FilePath] -- ^ archive directory contents
       -> Report
latest config contents =
    case categorize (isComplete config) (isInProgress config) contents of
      ((complete:_), [], _) -> Found (Just complete) [] []
      ([], inprogress, _)   -> Found Nothing inprogress []
      ((complete: _), inprogress, _) ->
          let (newer, older) = partition (\ip -> (date config) complete <= (date config) ip) inprogress
          in 
            Found (Just complete) newer older

getSnapshotDirectories :: (FilePath -> Bool) -- ^ only return directories that match this predicate (. and .. are automatically removed)
                       -> FilePath -- ^ path to directory containing snapshots
                       -> IO [FilePath]
getSnapshotDirectories nameP dir =
    do c <- liftM (filter (\fp -> (fp /= ".") && (fp /= "..") && nameP fp)) (getDirectoryContents dir)
       filterM (liftM isRealDirectory . getFileStatus . (dir +/+))  c
    where
      isRealDirectory :: FileStatus -> Bool
      isRealDirectory fs = all ($ fs) [isDirectory, not . isSymbolicLink ]

-- | file names should short lexagraphically
categorize :: (FilePath -> Bool) -> (FilePath -> Bool) -> [FilePath] -> ([FilePath], [FilePath], [FilePath])
categorize isCompleteP isInProgressP files = foldr category ([],[],[]) files
    where
      category file (complete, inprogress, other)
          | isCompleteP file = (insertBy newer file complete, inprogress, other)
          | isInProgressP file = (complete, insertBy newer file inprogress, other)
          | otherwise = (complete, inprogress, insertBy newer file other)
      newer = flip compare
