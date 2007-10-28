-- |Scan two directories (original and copy) and find all files in the
-- copy directory which are identical to a file in the original
-- directory.  This is done by sorting all the files by size, and then
-- comparing the files in each size group.
module Main where

import System.Directory
import System.IO
import System.Environment
import System.Posix.Files
import System.Posix.Types
import Data.List
import qualified Data.ByteString.Lazy as B

-- Lets try not to get these mixed up...
type CopyPath = FilePath
type OrigPath = FilePath

main =
    do args <- getArgs
       case args of
         [original, copy] ->
             do originals <- findFiles original >>= return . groupFiles
                candidates <- findFiles copy >>= return . groupFiles
                copies <- findCopies originals candidates
                mapM_ (hPutStrLn stderr . show) copies
         _ -> error $ "Usage: findcopies originaldir copydir"

-- |Return a list of all pairs where one file is in the copy directory and one is in the original.
findCopies :: [(FileOffset, [OrigPath])] -> [(FileOffset, [CopyPath])] -> IO [(OrigPath, CopyPath)]
findCopies _ [] = return []
findCopies [] _ = return []
findCopies originals@((originalSize, originalPaths) : moreOriginals) copies@((copySize, copyPaths) : moreCopies) =
    case compare originalSize copySize of
      GT -> findCopies moreOriginals copies
      LT -> findCopies originals moreCopies
      EQ -> do originalPairs <- mapM B.readFile originalPaths >>= return . zip originalPaths
               copyPairs <- mapM B.readFile copyPaths >>= return . zip copyPaths
               -- hPutStrLn stderr $ ("Size: " ++ show originalSize ++
               --                     ", originals: " ++ show (length originalPaths) ++
               --                     ", copies: " ++ show (length (copyPaths)))
               let matches = concat . map (findOriginal originalPairs) $ copyPairs
               -- case matches of [] -> return (); _ -> hPutStrLn stderr $ ("  Match: " ++ show matches)
               more <- findCopies moreOriginals moreCopies
               return (matches ++ more)
    where
      -- Compare the text of the copy to the text of each original, and return the
      -- path pair of the first match.
      findOriginal :: [(OrigPath, B.ByteString)] -> (CopyPath, B.ByteString) -> [(OrigPath, CopyPath)]
      findOriginal origPairs (copyPath, copyText) =
          maybe [] (\ pr -> [((fst pr), copyPath)]) (find ((==) copyText . snd) origPairs)

-- |Find all the non-directory files within a directory.  Returns a
-- list of each file's size and path.
findFiles :: FilePath -> IO [(FileOffset, FilePath)]
findFiles top =
    find ""
    where
      find path =
          -- Be very careful not to return a pair where one is a
          -- symlink to the other - you could lose both.
          do status <- getSymbolicLinkStatus (top ++ path)
             case isDirectory status of
               True -> do names <- getDirectoryContents (top ++ path) >>= return . filter (not . flip elem [".", ".."])
                          let subdirs = map ((path ++ "/") ++) names
                          mapM find subdirs >>= return . concat
               False -> return [(fileSize status, top ++ path)]

-- |Group files of equal size.
groupFiles :: [(FileOffset, FilePath)] -> [(FileOffset, [FilePath])]
groupFiles pairs =
    map unpair . groupBy (\ a b -> fst a == fst b) . sortBy (\ a b -> compare (fst b) (fst a)) $ pairs
    where
      unpair :: [(FileOffset, FilePath)] -> (FileOffset, [FilePath])
      unpair [] = error $ "Internal error"
      unpair ((size, path) : pairs) = (size, (path : map snd pairs))
