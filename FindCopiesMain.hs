module Main where

import System.Console.GetOpt
import System.Environment
import FindCopies

main =
    getArgs >>= doFind . computeFlags
    where
      computeFlags args =
          case getOpt Permute opts args of
            (o, [original, copy], []) -> (original, copy, o)
            (_, _, []) -> error usage
            (_, _, errs) -> error (concat errs ++ usage)
      doFind (original, copy, flags)
          | elem Help flags = putStrLn usage
          | elem PrintStatus flags = findCopies showStatus original copy
          | elem Counts flags = findCopies showCounts original copy
          | True = findCopies showStatus original copy
      usage = usageInfo "Usage: findCopes [options] originaldir copydir" opts

data Flag
    = Help
    | PrintStatus
    | Counts
    deriving Eq

opts :: [OptDescr Flag]
opts =
    [Option ['h'] ["help"] (NoArg Help) "Print help message and exit.",
     Option [] ["print-status"] (NoArg PrintStatus) "Print the status of each file.",
     Option [] ["counts"] (NoArg Counts) "Print the count of each status."]