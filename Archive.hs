module Archive 
    ( Option(..)
    , archive
    ) where

--import Control.Exception
import Data.List
import System.Time
import System.Locale
import System.Directory
import System.IO
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

removeIncomplete options backupDirectory =
    getDirectoryContents backupDirectory >>= return . filter isIncomplete  >>= mapM removeArchive
    where
      isIncomplete name = maybe False (== "incomplete.") . archivePrefix $ name
      removeArchive name = command options ("rm -rf " ++ backupDirectory ++ name)
      archivePrefix name =
          case matchRegex archiveNameRE name of
            Just [prefix, _] -> Just prefix
            Nothing -> Nothing
            _ -> error "internal error"

linkToLatest options backupDirectory today =
    do
      archives <- getDirectoryContents backupDirectory >>= return . filter isComplete
      case listToMaybe . sortBy (\ a b -> compare (archiveDate b) (archiveDate a)) $ archives of
        -- There is an outofdate archive from today, use it as is.
        Just archive | (backupDirectory ++ archive) == outofdate today ->
                return ()
        -- there is a finished archive from today, add the outofdate. prefix
        Just archive | (backupDirectory ++ archive) == destination today ->
                myRenameDirectory options (backupDirectory ++ archive) (outofdate today)
        -- The newest archive wasn't finished, rename it.
        Just archive | isPrefixOf "incomplete." archive ->
	        myRenameDirectory options (backupDirectory ++ archive) (outofdate today)
        Just archive ->
            -- The newest archive is finished and not from today, link to it.
            do command options ("cp -al " ++ backupDirectory ++ archive ++ " " ++ incomplete today)
               myRenameDirectory options (incomplete today) (outofdate today)
        -- There are no archives in the directory at all
        Nothing -> createDirectoryIfMissing True (outofdate today)
    where
      destination date = backupDirectory ++ date
      incomplete date = backupDirectory ++ "incomplete." ++ date
      isComplete name = maybe False (flip elem ["outofdate.", ""]) . archivePrefix $ name
      outofdate date = backupDirectory ++ "outofdate." ++ date

updateFromOriginal options original backupDirectory today =
    do
      let cmd = "rsync"
      let args = rsync options ++ ["-aHxSpDtl", "--partial", "--delete", "--recursive",
                                   "--delete-excluded", "--stats", original, (outofdate today)]
      hPutStrLn stderr ("> " ++ cmd)
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
            do
              myRenameDirectory options (outofdate today) (destination today)
              -- We would like to mark archives that are identical to
              -- others, but at this point we don't know what the
              -- original archive was.  Probably need to pass another
              -- argument in.
              case matchRegex sizeRE . byteStringToString . B.concat . stdoutOnly $ output of
                Just ["0"] -> hPutStrLn stderr "Zero bytes transferred."
                _ -> hPutStrLn stderr "More than zero bytes transferred."
    where
      sizeRE = mkRegex "Total transferred file size: ([0-9]*) bytes"
      rsync (Rsync x : options) = x : rsync options
      rsync (_ : options) = rsync options
      rsync [] = []
      destination date = backupDirectory ++ date
      outofdate date = backupDirectory ++ "outofdate." ++ date
      doOutput x@(Stdout s) = do B.hPut stdout s; return x
      doOutput x@(Stderr s) = do B.hPut stderr s; return x
      doOutput x = return x

byteStringToString :: B.ByteString -> String
byteStringToString b = map (chr . fromInteger . toInteger) . B.unpack $ b

myRenameDirectory options old new =
    if elem DryRun options then
        hPutStrLn stderr ("renameDirectory " ++ show old ++ " " ++ show new) else
        renameDirectory old new

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
