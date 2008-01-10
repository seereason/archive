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

import Control.Exception
import Data.List
--import System.Time
--import System.Locale
--import System.Directory
import System.IO
import Data.Maybe
import System.Environment
--import System.Cmd
--import System.Exit
--import Text.Regex
--import Data.Char(chr)
--import Data.Char
--import qualified Data.ByteString as B
--import Linspire.Unix.Process
import Archive
import Extra.Help
import System.Console.GetOpt

opts :: [OptDescr [Option]]
opts =
    [ Option [] ["prune"] (ReqArg (\n -> [Prune n]) "NUM") "limit the number of backup dirs to NUM."
    , Option [] ["unlink"] (NoArg [Unlink]) "Keep only the most recent hard link. The newest backup is always complete, but the previous day will only include the files that changed or were removed."
    , Option [] ["current"] (NoArg [Current]) "Create a link named 'current' to the new archive."
    , Option [] ["exclude"] (ReqArg (\x -> [Rsync "--exclude", Rsync x]) "PATTERN") "Passed to rsync. Implies rsync's --delete-excluded flag (so that adding this flag makes files go away in newer backups)."
    , Option ['n'] ["dry-run"] (NoArg [DryRun, Rsync "-n"]) "Do not do any file transfers, just report what would have happened."
    , Option ['v'] ["verbose"] (NoArg [Rsync "-v"]) "run rsync with verbose option."
    , Option ['P'] [] (NoArg [Rsync "-P"]) "run rsync with -P, which is the same as --partial --progress."
    , Option ['c'] ["checksum"] (NoArg [Rsync "-c"]) "run rsync with -c, skip based on checksum, not mod-time & size."
    , Option [] ["delete-excluded"] (NoArg [Rsync "--delete-excluded"]) "run rsync with --delete-excluded, also delete excluded files from dest dirs."
    , Option [] ["delete-after"] (NoArg [Rsync "--delete-after"]) "run rsync with --delete-after, Request  that  the file-deletions on the receiving side be done after the transfer has completed."
    , Option [] ["partial"] (NoArg [Rsync "--partial"]) "run rsync with --partial, keep partially transferred files."
    , Option [] ["force"] (NoArg [Rsync "--force"]) "run rsync with --force, force deletion of dirs even if not empty."
    , Option [] ["size-only"] (NoArg [Rsync "--size-only"]) "run rsync with --size-only, skip files that match in size."
    , Option [] ["timeout"] (ReqArg (\t -> [Rsync $ "--timeout="++ t]) "TIME") "set I/O timeout in seconds."
    , Option [] ["bwlimit"] (ReqArg (\kbps -> [Rsync $ "--bwlimit=" ++ kbps]) "KBPS") "limit I/O bandwidth; KBytes per second."
    ]

manpage =
    Manpage { name	  	= "archive"
            , sectionNum  	= General
            , shortDesc	  	= text "create incremental backups of directories using rsync and hardlinks."
            , synopsis	  	= text "archive [options] original backupdir"
            , description 	= text "Create a backup of ORIGINAL in BACKUPDIR in a directory whose name is todays date.\
                                       \The original may be on a remote machine.\
                                       \This is achieved without wasting disk space on unchanged files using\
                                       \a simple incremental backup technique I read about somewhere using\
                                       \cp -al to create a hard linked copy of the previous backup and rsync\
                                       \to modify that copy into a copy of the current directory.  It does use\
                                       \a lot of inodes, but I haven't run out yet."
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
       case parseOptions args of
         (Left e) -> error (e ++ (usageInfo synopsis opts))
         (Right (options, original, backup)) ->
              do res <- archive options original backup
                 case res of
                   (Left e) -> error (show e)
                   (Right r) -> hPutStrLn stderr (show r)
    where
      synopsis = "archive [options] original backupdir"

-- * JAS - Old Stuff, just here for reference until I am sure things are the same in the new system

oldGetOptions :: IO ([Option], String, String)
oldGetOptions =
    do args <- getArgs
       case processArgs ([], Nothing, Nothing) args of
         (_, Nothing, _) -> error $ "Missing source and destination arguments"
         (_, _, Nothing) -> error $ "Missing destination argument"
         (options, Just original, Just backup) -> return (options, original, backup)
    where
      processArgs (a, o, b) [] = (a, o, b)
      processArgs (a, o, b) ("--prune" : x : xs) = processArgs (Prune x : a, o, b) xs
      processArgs (a, o, b) ("--unlink" : xs) = processArgs (Unlink : a, o, b) xs
      processArgs (a, o, b) ("--current" : xs) = processArgs (Current : a, o, b) xs
      processArgs (a, o, b) ("--exclude" : x : xs) = processArgs (Rsync "--exclude" : Rsync x : a, o, b) xs
      processArgs (a, o, b) ("-n" : xs) = processArgs (DryRun : Rsync "-n" : a, o, b) xs
      processArgs (a, o, b) ("--dry-run" : xs) = processArgs (DryRun : Rsync "-n" : a, o, b) xs
      processArgs (a, o, b) (x : xs) | elem x rsyncArgs = processArgs (Rsync x : a, o, b) xs
      processArgs (a, o, b) (x : xs) | isPrefixOf "--timeout=" x = processArgs (Rsync x : a, o, b) xs
      processArgs (a, o, b) (x : xs) | isPrefixOf "--bwlimit=" x = processArgs (Rsync x : a, o, b) xs
      processArgs (a, Nothing, b) (x : xs) = processArgs (a, Just x, b) xs
      processArgs (a, o, Nothing) (x : xs) = processArgs (a, o, Just x) xs
      processArgs _ (x : _) = error $ "Unexpected argument: " ++ x

rsyncArgs = ["-v", "-P", "-c", "--delete-excluded", "--delete-after", "--partial", "--force", "--size-only"]



oldusage :: String -> IO String
oldusage message =
    do
      putStrLn . concat . intersperse "\n" $
        [message,
         "",
         "Usage:",
         "   archive [options] original backupdir",
         "",
         "Options:",
         "  --prune <number>	-- limit the number of backup dirs to <number>",
         "  --unlink		-- Keep only the most recent hard link.  The newest",
         "		   backup is always complete, but the previous day",
         "		   will only include the files that changed or were",
         "		   removed.",
         "  -A			-- Always create a new backup, without this option",
         "		   today's backup will be updated if it already exists",
         "  --exclude		-- Passed to rsync, implies rsync's --delete-excluded",
         "		   flag (so that adding this flag makes files go away",
         "		   in newer backups.)",
         "  --current		-- Create a link named 'current' to the new archive.",
         "",

         "",
         "Example crontab:",
         " 0 1 * * * /root/bin/archive --prune 20 --exclude '/.mozilla/**/Cache/' --exclude '/.kde/share/cache/' root@dsf:/home/dsf /backups/dsf",
         " 0 2 * * * /root/bin/archive --prune 20 root@p4:/home/audio /backups/audio",
         " 20 2 * * * /root/bin/archive --prune 10 root@p4:/disks/hdc3/cdroms /backups/cdroms",
         " 30 2 * * * /root/bin/archive --exclude '/.mozilla/**/Cache/' --exclude '/.kde/share/cache/' root@t22:/root /backups/ldt-t22",
         " 40 2 * * * /root/bin/archive root@dsf:/var/lib/geneweb /backups/geneweb"]
      return message
