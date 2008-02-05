module System.Archive.UpdateMirror 
    ( updateMirrorMain
    , Option(..)
    , Target(..)
    , genericConfig
    ) 
    where

import Control.Monad
import Data.List
import Extra.Help as H
import Extra.HughesPJ
import System.Console.GetOpt
import System.Environment
import System.IO

import System.Archive.Archive
import System.Archive.Target as T

-- * General Stuff

manpage progName targets =
    Manpage { name		= progName 
            , sectionNum	= General
            , shortDesc		= text "tool to keep mirrors of various repositories up to date."
            , synopsis		= text (progName ++ " TARGET...")
            , description	= text "update the mirrors named on the command line."
            , H.options		= Just opts
            , extraSections	= Just [targetSection]
            , files		= Nothing
            , environment	= Nothing
            , diagnostics	= Nothing
            , bugs		= Nothing
            , authors		= Just [("Jeremy Shaw", "jeremy.shaw@linspire.com")]
            , seeAlso		= Nothing
            }
        where
          targetSection :: (ShowIn, Text, Elements)
          targetSection = (InBoth, (text "TARGETS"), (showTargets targets))



opts :: [OptDescr [Option]]
opts =
    [ -- Option [] ["prune"] (ReqArg (\n -> [Prune n]) "NUM") "limit the number of backup dirs to NUM."
    -- , Option [] ["unlink"] (NoArg [Unlink]) "Keep only the most recent hard link. The newest backup is always complete, but the previous day will only include the files that changed or were removed."
    -- , Option [] ["current"] (NoArg [Current]) "Create a link named 'current' to the new archive."
    Option [] ["exclude"] (ReqArg (\x -> [Rsync "--exclude", Rsync x]) "PATTERN") "Passed to rsync. Implies rsync's --delete-excluded flag (so that adding this flag makes files go away in newer backups)."
    -- , Option ['n'] ["dry-run"] (NoArg [DryRun, Rsync "-n"]) "Do not do any file transfers, just report what would have happened."
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
    , Option [] ["dump-man-page"] (NoArg []) "dump the manpage for this program on stdout and exit immediately. Use groff -mandoc to process the output."
    , Option [] ["no-update-symlink"] (NoArg [NoUpdateSymlink]) "do not update the symlink, current, after the update is done."
    ]

parseOptions :: [Target] -> [String] -> Either String ([Option], [String])
parseOptions targets args =
    case getOpt Permute opts args of
         (extraOptions, tgts@(_:_), []) ->
             case tgts \\ (map prettyName targets) of
               [] -> Right (concat extraOptions, tgts)
               unknownTargets -> Left $ (if (singleton unknownTargets) 
                                        then "Unrecognized target: " 
                                        else "Unrecognized targets: ") ++ 
                                 show unknownTargets
         (_, [], errors) -> Left $ concat $ "You must specify one or more TARGETs.\n" : errors
         (_, _, errors) -> Left $ concat $ errors
    where
      singleton [_] = True
      singleton _ = False

updateMirrorMain :: [Target] -> IO ()
updateMirrorMain targets = 
    do args <- getArgs
       progName <- getProgName
       when ("--dump-man-page" `elem` args) (dumpManPage (manpage progName targets))
       case parseOptions targets args of
         (Left e) -> do hPutStrLn stderr e
                        hPutStrLn stderr =<< usage (manpage progName targets)
         (Right (extraOptions, tgts)) ->
             do res <- archiveTargets extraOptions (filter (\t -> (prettyName t) `elem` tgts) targets)
                putStrLn =<< renderWidth (ppResults res)
