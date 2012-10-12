-- |The archive command needs to run as root so it can create any file
-- with any ownership, special files, etc.  This makes it a security
-- risk.  It should use a configuration file to set the destination
-- directory so a malicious user can't pass it arguments to destroy
-- other parts of the system.
-- 
-- The command
--
--   archive <user>@<host>:<path> <dest>
--
-- Creates a copy of the directory in the first argument at
--
--   <dest>/<date>
module Main where

import Control.Monad
--import Data.List
import System.IO
--import Data.Maybe
import System.Environment
import System.Exit
import System.Archive.Archive
import Text.Help

opts :: [OptDescr [Option]]
opts =
    [ -- Option [] ["prune"] (ReqArg (\n -> [Prune n]) "NUM") "limit the number of backup dirs to NUM."
    -- , Option [] ["unlink"] (NoArg [Unlink]) "Keep only the most recent hard link. The newest backup is always complete, but the previous day will only include the files that changed or were removed."
    -- , Option [] ["current"] (NoArg [Current]) "Create a link named 'current' to the new archive."
    Option [] ["exclude"] (ReqArg (\x -> [Rsync "--exclude", Rsync x]) "PATTERN") (text "Passed to rsync. Implies rsync's --delete-excluded flag (so that adding this flag makes files go away in newer backups).")
    -- , Option ['n'] ["dry-run"] (NoArg [DryRun, Rsync "-n"]) "Do not do any file transfers, just report what would have happened."
    , Option ['v'] ["verbose"] (NoArg [Rsync "-v"]) (text "run rsync with verbose option.")
    , Option ['P'] [] (NoArg [Rsync "-P"]) (text "run rsync with -P, which is the same as --partial --progress.")
    , Option ['c'] ["checksum"] (NoArg [Rsync "-c"]) (text "run rsync with -c, skip based on checksum, not mod-time & size.")
    , Option [] ["delete-excluded"] (NoArg [Rsync "--delete-excluded"]) (text "run rsync with --delete-excluded, also delete excluded files from dest dirs.")
    , Option [] ["delete-after"] (NoArg [Rsync "--delete-after"]) (text "run rsync with --delete-after, Request  that  the file-deletions on the receiving side be done after the transfer has completed.")
    , Option [] ["partial"] (NoArg [Rsync "--partial"]) (text "run rsync with --partial, keep partially transferred files.")
    , Option [] ["force"] (NoArg [Rsync "--force"]) (text "run rsync with --force, force deletion of dirs even if not empty.")
    , Option [] ["size-only"] (NoArg [Rsync "--size-only"]) (text "run rsync with --size-only, skip files that match in size.")
    , Option [] ["timeout"] (ReqArg (\t -> [Rsync $ "--timeout="++ t]) "TIME") (text "set I/O timeout in seconds.")
    , Option [] ["bwlimit"] (ReqArg (\kbps -> [Rsync $ "--bwlimit=" ++ kbps]) "KBPS") (text "limit I/O bandwidth; KBytes per second.")
    , Option [] ["no-update-symlink"] (NoArg [NoUpdateSymlink]) (text "do not automatically update the symlink named 'current' to point the latest snapshot.")
    , Option [] ["dump-man-page"] (NoArg []) (text "dump the manpage for this program on stdout and exit immediately. Use groff -mandoc to process the output.")
    ]

manpage =
    Manpage { name	  	= "archive"
            , sectionNum  	= General
            , shortDesc	  	= text "create incremental backups of directories using rsync and hardlinks."
            , synopsis	  	= text "archive [options] original backupdir"
            , description 	= text "Create a backup of " <> i <> text "ORIGINAL" <> p <> text " in " <> i <> text "BACKUPDIR" <> p <> 
                                  text" in a directory whose name is todays date. \
                                       \The original may be on a remote machine. \
                                       \This is achieved without wasting disk space on unchanged files using \
                                       \a simple incremental backup technique I read about somewhere using " <>
                                       cw <> text "cp -al" <> 
                                       p <> text" to create a hard linked copy of the previous backup and rsync \
                                       \to modify that copy into a copy of the current directory.  It does use \
                                       \a lot of inodes, but I haven't run out yet on Reiser 3."
            , options		= Just opts
            , extraSections	= Nothing
            , files		= Nothing
            , environment	= Nothing
            , diagnostics	= Nothing
            , bugs		= Nothing
            , authors		= Just [ ("David Fox","david@seereason.org")
                                       , ("Jeremy Shaw", "jeremy@n-heptane.com")
                                       ]
            , seeAlso = Nothing
            }

parseOptions args =
       case getOpt Permute opts args of
         (options, [src,dest], []) -> Right (concat options, src, dest)
         (_,[],errors) ->  Left $ concat $ "Missing original and backupdir arguments.\n" : errors
         (_,[_],errors) -> Left $ concat $ "Missing backupdir argument\n" : errors
         (_,(_:_:rest),errors) -> Left $ concat $ ("Unexpected arguments: " ++ unwords rest ++ "\n") : errors

main :: IO ()
main =
    do args <- getArgs
       when ("--dump-man-page" `elem` args) (dumpManPage manpage)
       case parseOptions args of
         (Left e) -> hPutStrLn stderr e >>
                    usage manpage >>= hPutStrLn stderr >> 
                    exitFailure
         (Right (options, original, backup)) ->
             do archive (genericConfig "snapshot" "%Y-%m-%d") options original backup []
                return ()
      
