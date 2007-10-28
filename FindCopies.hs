-- |Scan two directories (original and copy) and find all files in the
-- copy directory which are identical to a file in the original
-- directory.  This is done by sorting all the files by size, and then
-- comparing the files in each size group.
module Main where

import Control.Exception
import System.Directory
import System.IO
import System.Environment
import System.Posix.Files
import System.Posix.Types
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString as B

-- Lets try not to get these mixed up...
type CopyPath = FilePath
type OrigPath = FilePath

main =
    do args <- getArgs
       case args of
         [original, copy] ->
             do originals <- findFiles original >>= return . groupFiles
                candidates <- findFiles copy >>= return . groupFiles
                findCopies (hPutStrLn stderr . show) originals candidates
         _ -> error $ "Usage: findcopies originaldir copydir"

-- |Return a list of all pairs where one file is in the copy directory and one is in the original.
findCopies :: ((OrigPath, CopyPath) -> IO ()) -> [(FileOffset, [OrigPath])] -> [(FileOffset, [CopyPath])] -> IO ()
findCopies _ _ [] = return ()
findCopies _ [] _ = return ()
findCopies f originals@((originalSize, originalPaths) : moreOriginals) copies@((copySize, copyPaths) : moreCopies) =
    case compare originalSize copySize of
      GT -> findCopies f moreOriginals copies
      LT -> findCopies f originals moreCopies
      EQ -> compareFiles originalPaths copyPaths >>
            findCopies f moreOriginals moreCopies
    where
      compareFiles originalPaths copyPaths =
          do originalHandles <- mapM open originalPaths
             originalText <- mapM read originalHandles
             copyHandles <- mapM open copyPaths
             copyText <- mapM read copyHandles
             let originalTriples = catMaybes . map zapMissing $ zip3 originalPaths originalHandles originalText
             let copyTriples = catMaybes . map zapMissing $ zip3 copyPaths copyHandles copyText
             let matches = concat . map (findOriginal originalTriples) $ copyTriples
             mapM_ f matches
             mapM_ (\ (_, handle, _) -> hClose handle) (originalTriples ++ copyTriples)
      -- Compare the text of the copy to the text of each original, and return the
      -- path pair of the first match.
      findOriginal :: [(OrigPath, Handle, B.ByteString)] -> (CopyPath, Handle, B.ByteString) -> [(OrigPath, CopyPath)]
      findOriginal origTriples (copyPath, _, copyText) =
          maybe [] (\ (path, _, _) -> [(path, copyPath)]) (find (\ (_, _, text) -> text == copyText) origTriples)
      open :: FilePath -> IO (Maybe Handle)
      open path = try (openBinaryFile path ReadMode) >>= either (return . const Nothing) (return . Just)
      read :: Maybe Handle -> IO (Maybe B.ByteString)
      read = maybe (return Nothing) (\ h -> B.hGetContents h >>= return . Just)
      omitFailed :: [(FilePath, Maybe Handle, Maybe B.ByteString)] -> [(FilePath, Handle, B.ByteString)]
      omitFailed = catMaybes . map zapMissing
      zapMissing (a, Just b, Just c) = Just (a, b, c)
      zapMissing _ = Nothing

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
