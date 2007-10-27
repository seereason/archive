module Archive 
    ( Option(..)
    , UpdateResult(..)
    , archive
    ) where

import Data.List
import System.FilePath
import System.Time
import System.Locale
import System.Directory
import System.IO
import System.IO.Error
import Data.Maybe
--import System.Environment
import System.Exit
import Text.Regex
import Data.Char(chr)
--import Data.Char
import qualified Data.ByteString as B
import Linspire.Unix.Process

-- |These represent the command line options to this program.
data Option
    = DryRun		-- |Print the commands to execute, done run them
    | Unlink		-- |Unimplemented
    | Current		-- |Unimplemented
    | Prune String	-- |Unimplemented
    | Rsync String	-- |Pass one or more additional arguments to the
			-- rsync sub-process
    deriving (Eq, Show)

{-
archive :: [Option] -> String -> String -> IO ()
archive options original backupDirectory =
    cleanFailed >>= createCopy >>= updateCopy
    where
      cleanFailed :: IO (Either [String] ())
      cleanFailed = removeIncomplete options backupDirectory
-}

newtype Date = Date String deriving Show

archive :: [Option] -> String -> String -> IO (Either IOError UpdateResult)
archive options original backupDirectory =
    -- Remove any incomplete archives.  These are left by runs that died
    -- while the older archive was being copied.
    removeIncomplete options backupDirectory' >>
    -- Get the timeofday to decide where to put the archive
    getClockTime >>= toCalendarTime >>= return . Date . formatCalendarTime defaultTimeLocale "%Y%m%d" >>=
    -- Create a copy of the latest complete archive with today's
    -- date and the outofdate prefix.
    linkToLatest options backupDirectory' >>=
    -- Rsync with the remote directory.
    either (return . Left) (updateFromOriginal options original' backupDirectory')
    where
      backupDirectory' = dropTrailingPathSeparator backupDirectory
      original' = dropTrailingPathSeparator original

removeIncomplete options backupDirectory =
    try (getDirectoryContents backupDirectory) >>=
    either (\ e -> if isDoesNotExistError e then
                       return (Right ()) else
                       return (Left ("removeIncomplete: Unexpected exception " ++ show e)))
               (\ r -> mapM_ removeArchive (filter isIncomplete r) >>= return . Right)
    where
      isIncomplete name = maybe False (== "incomplete.") . archivePrefix $ name
      removeArchive name = command options ("rm -rf " ++ backupDirectory ++ name)
      archivePrefix name =
          case matchRegex archiveNameRE name of
            Just [prefix, _] -> Just prefix
            Nothing -> Nothing
            _ -> error "internal error"

-- Create a new archive which is hard linked to the most recent complete archive.
linkToLatest :: [Option] -> FilePath -> Date -> IO (Either IOError Date)
linkToLatest options dest today =
    -- Get a list of all the archives that are not incomplete, that
    -- is, completed links to a previous archive, but not necessarily
    -- fully updated from the original.
    try (getDirectoryContents dest >>= return . filter isComplete) >>=
    either (\ e -> if isDoesNotExistError e then doLink [] else return (makeLeft "linkToLatest 0: " e)) doLink
    where
      -- Given a list of existing archives, create a new archive hard linked to the newest existing archive.
      doLink names =
          hPutStrLn stderr ("doLink " ++ show names) >> hFlush stderr >>
          case listToMaybe . sortBy (\ a b -> compare (archiveDate b) (archiveDate a)) $ names of
            Just newest
                -- There is already an archive from today marked outofdate, use it as is.
                | (dest ++ "/" ++ newest) == outofdate dest today ->
                    return (Right today)
                -- there is a finished archive from today, mark it outofdate and proceed.
                | (dest ++ "/" ++ newest) == finished dest today ->
                    try (renameDirectory (dest ++ "/" ++ newest) (outofdate dest today)) >>=
                    return . either (makeLeft "linkToLatest 1: ") (const (Right today))
                -- The newest archive is finished and not from today, create a new link to it.
                | True ->
                    do let cmd = ("cp -al " ++ dest ++ "/" ++ newest ++ " " ++ incomplete dest today)
                       result <- lazyCommand cmd []
                       case exitCodeOnly result of
                         ExitSuccess : _ -> 
                             try (renameDirectory (incomplete dest today) (outofdate dest today)) >>=
                             return . either (makeLeft "linkToLatest 3: ") (const (Right today))
                         _ -> return . Left . userError $ "Failure in linkToLatest: " ++ cmd
            -- There are no archives in the directory at all.
            Nothing -> hPutStrLn stderr ("creating " ++ show (outofdate dest today)) >>
                       try (createDirectoryIfMissing True (outofdate dest today)) >>=
                       return . either
                                  (\ e -> Left (userError ("Could not create " ++ outofdate dest today ++ ": " ++ show e)))
                                  (const (Right today))
      makeLeft s e = Left . userError $ s ++ show e

data UpdateResult = NoChanges | Changes deriving Show

updateFromOriginal :: [Option] -> FilePath -> FilePath -> Date -> IO (Either IOError UpdateResult)
updateFromOriginal options original dest today =
    do
      let cmd = "rsync"
      let args = rsync options ++ ["-aHxSpDtl", "--partial", "--delete", "--recursive",
                                   "--delete-excluded", "--stats", original ++ "/", (outofdate dest today)]
      hPutStrLn stderr ("> " ++ concat (intersperse " " (cmd : args)))
      hPutStr stderr "  Updating from original ... "
      output <- if elem DryRun options then
                    return [Result ExitSuccess] else
                    lazyProcess cmd args Nothing Nothing [] >>= mapM doOutput
      case exitCodeOnly output of
        [] ->
            error "No exit status from rsync process!?"
        ExitFailure n : _ ->
            do
              hPutStrLn stderr ("Exiting with code " ++ show n)
              exitWith (ExitFailure n)
        ExitSuccess : _ ->
            try (renameDirectory (outofdate dest today) (finished dest today)) >>=
            return . either
                       (\ e -> Left (userError ("updateFromOriginal: failed to rename: " ++
                                                     outofdate dest today ++ " -> " ++ finished dest today ++ ": " ++ show e)))
                       (\ _ ->
                        -- We would like to mark archives that are identical to
                        -- others, but at this point we don't know what the
                        -- original archive was.  Probably need to pass another
                        -- argument in.
                        case matchRegex sizeRE . byteStringToString . B.concat . stdoutOnly $ output of
                          Just ["0"] -> Right NoChanges
                          _ -> Right Changes)
    where
      sizeRE = mkRegex "Total transferred file size: ([0-9]*) bytes"
      rsync (Rsync x : options) = x : rsync options
      rsync (_ : options) = rsync options
      rsync [] = []
      doOutput x@(Stdout s) = do B.hPut stdout s; return x
      doOutput x@(Stderr s) = do B.hPut stderr s; return x
      doOutput x = return x

byteStringToString :: B.ByteString -> String
byteStringToString b = map (chr . fromInteger . toInteger) . B.unpack $ b

command options s =
          do hPutStrLn stderr ("> " ++ s)
             if elem DryRun options then
                 return () else
                 do
                   result <- lazyCommand s []
                   case exitCodeOnly result of
                     ExitSuccess : _ -> return ()
                     failure@(ExitFailure _) : _ -> exitWith failure
                     [] -> exitWith (ExitFailure 1)

archiveDate name =
          case matchRegex archiveNameRE name of
            Just [_, date] -> Just date
            Nothing -> Nothing
            _ -> error "internal error"
archivePrefix name =
          case matchRegex archiveNameRE name of
            Just [prefix, _] -> Just prefix
            Nothing -> Nothing
            _ -> error "internal error"
archiveNameRE = mkRegex "^(.*)([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9])$"

-- Path constructors: "Incomplete" means an interruption occurred
-- while linking to the previous archive, such a directory should not
-- be used.  "Outofdate" means an interruption occurred while updating
-- from the original, an update of this directory can be resumed
-- safely.  "Finished" is the location of the completed backup.
incomplete dir (Date date) = dir ++ "/incomplete." ++ date
outofdate dir (Date date) = dir ++ "/outofdate." ++ date
finished dir (Date date) = dir ++ "/" ++ date

isComplete = not . isPrefixOf "incomplete."
