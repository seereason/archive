module Archive 
    ( Option(..)
    , UpdateResult(..)
    , archive
    ) where

import Data.List
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
    cleanup >>
    -- Get the timeofday to decide where to put the archive
    getDate >>=
    -- Create a copy of the latest complete archive with today's
    -- date and the outofdate prefix.
    linkCopy >>=
    -- Rsync with the remote directory.
    updateCopy
    where
      cleanup = removeIncomplete options backupDirectory
      getDate = getClockTime >>= toCalendarTime >>= return . Date . formatCalendarTime defaultTimeLocale "%Y%m%d"
      linkCopy :: Date -> IO (Either IOError Date)
      linkCopy today = linkToLatest options backupDirectory today
      updateCopy :: Either IOError Date -> IO (Either IOError UpdateResult) 
      updateCopy (Left message) = return (Left message)
      updateCopy (Right today) = updateFromOriginal options original backupDirectory today

{-
archive :: [Option] -> String -> String -> IO ()
archive options original backupDirectory =
    do today <- getClockTime >>= toCalendarTime >>= return . formatCalendarTime defaultTimeLocale "%Y%m%d"
       -- If the ALWAYS flag is set we may need to create a directory with hours
       -- minutes and seconds in the name.
       hPutStrLn stderr (original ++ " -> " ++ backupDirectory ++ today)
       createDirectoryIfMissing True backupDirectory
       -- Remove any incomplete archives.  These are left by runs that died
       -- while the older archive was being copied.
       removeIncomplete options backupDirectory
       -- Create a copy of the latest complete archive with today's
       -- date and the outofdate prefix.
       linkToLatest options backupDirectory today
       -- Now we need to do the rsync from the remote directory.
       updateFromOriginal options original backupDirectory today
-}

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

--removeIncomplete :: [Option] -> String -> IO ()
{-
removeIncomplete options backupDirectory =
    getDirectoryContents backupDirectory >>= return . filter isIncomplete  >>= mapM removeArchive
{-
    do result <- try (getDirectoryContents backupDirectory)
       let dirs = case result of
                    (Left e) -> if isDoesNotExistError e then [] else error ("Unexpected excption: " ++ show e)
                    (Right r) -> r
       mapM_ removeArchive (filter isIncomplete dirs) >>= return . Right
-}
    where
      isIncomplete name = maybe False (== "incomplete.") . archivePrefix $ name
      removeArchive name = command options ("rm -rf " ++ backupDirectory ++ name)
      archivePrefix name =
          case matchRegex archiveNameRE name of
            Just [prefix, _] -> Just prefix
            Nothing -> Nothing
            _ -> error "internal error"
-}

linkToLatest :: [Option] -> FilePath -> Date -> IO (Either IOError Date)
linkToLatest options backupDirectory today =
    try (getDirectoryContents backupDirectory >>= return . filter isComplete) >>=
    either (\ e -> if isDoesNotExistError e then doLink [] else return (makeLeft "linkToLatest 0: " e)) doLink
    where
      doLink archives =
          hPutStrLn stderr ("doLink " ++ show archives) >> hFlush stderr >>
          case listToMaybe . sortBy (\ a b -> compare (archiveDate b) (archiveDate a)) $ archives of
            Just archive
                -- There is an outofdate archive from today, use it as is.
                | (backupDirectory ++ archive) == outofdate today ->
                    return (Right today)
                -- there is a finished archive from today, add the outofdate. prefix
                | (backupDirectory ++ archive) == destination today ->
                    try (renameDirectory (backupDirectory ++ archive) (outofdate today)) >>=
                    return . either (makeLeft "linkToLatest 1: ") (const (Right today))
                -- The newest archive wasn't finished, rename it.
                | isPrefixOf "incomplete." archive ->
	            try (renameDirectory (backupDirectory ++ archive) (outofdate today)) >>=
                    return . either (makeLeft "linkToLatest 2: ") (const (Right today))
                -- The newest archive is finished and not from today, link to it.
                | True ->
                    do let cmd = ("cp -al " ++ backupDirectory ++ archive ++ " " ++ incomplete today)
                       result <- lazyCommand cmd []
                       case exitCodeOnly result of
                         ExitSuccess : _ -> 
                             try (renameDirectory (incomplete today) (outofdate today)) >>=
                             return . either (makeLeft "linkToLatest 3: ") (const (Right today))
                         _ -> return . Left . userError $ "Failure in linkToLatest: " ++ cmd
            -- There are no archives in the directory at all
            Nothing -> hPutStrLn stderr ("creating " ++ show (outofdate today)) >>
                       try (createDirectoryIfMissing True (outofdate today)) >>=
                       --(\ x -> do hPutStrLn stderr (show x); return x) >>=
                       return . either
                                  (\ e -> Left (userError ("Could not create " ++ outofdate today ++ ": " ++ show e)))
                                  (const (Right today))
      destination (Date date) = backupDirectory ++ date
      incomplete (Date date) = backupDirectory ++ "incomplete." ++ date
      isComplete name = maybe False (flip elem [".outofdate.", ""]) . archivePrefix $ name
      outofdate (Date date) = backupDirectory ++ ".outofdate." ++ date
      makeLeft s e = Left . userError $ s ++ show e

data UpdateResult = NoChanges | Changes deriving Show

updateFromOriginal :: [Option] -> FilePath -> FilePath -> Date -> IO (Either IOError UpdateResult)
updateFromOriginal options original backupDirectory today =
    do
      let cmd = "rsync"
      let args = rsync options ++ ["-aHxSpDtl", "--partial", "--delete", "--recursive",
                                   "--delete-excluded", "--stats", original, (outofdate today)]
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
            try (renameDirectory (outofdate today) (destination today)) >>=
            return . either
                       (\ e -> Left (userError ("updateFromOriginal: failed to rename: " ++
                                                     outofdate today ++ " -> " ++ destination today ++ ": " ++ show e)))
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
      destination (Date date) = backupDirectory ++ date
      outofdate (Date date) = backupDirectory ++ ".outofdate." ++ date
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
