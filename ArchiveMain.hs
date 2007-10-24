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

main :: IO ()
main =
    try getOptions >>=
    either (\ e -> usage (show e) >> error (show e))
           (\ (options, original, backup) -> archive options original backup)

getOptions :: IO ([Option], String, String)
getOptions =
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

usage :: String -> IO String
usage message =
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
         "Create a backup of ORIGINAL in BACKUPDIR in a directory whose name is",
         "todays date.  The original may be on a remote machine.",
         "",
         "This is achieved without wasting disk space on unchanged files using",
         "a simple incremental backup technique I read about somewhere using",
         "cp -al to create a hard linked copy of the previous backup and rsync",
         "to modify that copy into a copy of the current directory.  It does use",
         "a lot of inodes, but I haven't run out yet.",
         "",
         "Example crontab:",
         " 0 1 * * * /root/bin/archive --prune 20 --exclude '/.mozilla/**/Cache/' --exclude '/.kde/share/cache/' root@dsf:/home/dsf /backups/dsf",
         " 0 2 * * * /root/bin/archive --prune 20 root@p4:/home/audio /backups/audio",
         " 20 2 * * * /root/bin/archive --prune 10 root@p4:/disks/hdc3/cdroms /backups/cdroms",
         " 30 2 * * * /root/bin/archive --exclude '/.mozilla/**/Cache/' --exclude '/.kde/share/cache/' root@t22:/root /backups/ldt-t22",
         " 40 2 * * * /root/bin/archive root@dsf:/var/lib/geneweb /backups/geneweb"]
      return message
