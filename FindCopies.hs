-- |Scan two directories (original and copy) and find all files in the
-- copy directory which are identical to a file in the original
-- directory.  This is done by sorting all the files by size, and then
-- comparing the files in each size group.
module FindCopies
    ( Status
    , findCopies
    , findPaths
    , showStatus
    , showCounts
    , showCommon
    ) where

import Control.Exception
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Files
import System.Posix.Types
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString as B

-- Lets try not to get these mixed up...
type CopyPath = FilePath
type OrigPath = FilePath

data Status = NoOriginal FilePath
            | SameNameAndContents FilePath FilePath
            | SameContents FilePath FilePath
            | SameFile FilePath FilePath 
            | Empty FilePath
            | SpecialFile FilePath
            | SymbolicLink FilePath
              deriving Show

-- |Compare the text of the copy to the text of each original, and return the
-- path pair of the first match.
getStatus :: [FileInfo] -> FileInfo -> Status
getStatus originalCandidates copyInfo@(copyPath, _, _, _) =
    foldl foldStatus (NoOriginal copyPath) (map (pairStatus copyInfo) originalCandidates)
    where
      pairStatus :: FileInfo -> FileInfo -> Status
      pairStatus (copyPath, copyStatus, _, copyText) (origPath, origStatus, _, origText)
          | isSymbolicLink copyStatus = SymbolicLink copyPath
          | isBlockDevice copyStatus || isCharacterDevice copyStatus || isNamedPipe copyStatus || isSocket copyStatus = SpecialFile copyPath
          | copyText == B.empty = Empty copyPath
          | deviceID copyStatus == deviceID origStatus && fileID copyStatus == fileID origStatus = SameFile copyPath origPath
          | copyText == origText && takeBaseName copyPath == takeBaseName origPath = SameNameAndContents copyPath origPath
          | copyText == origText = SameContents copyPath origPath
          | True = NoOriginal copyPath
      foldStatus :: Status -> Status -> Status
      foldStatus s@(SymbolicLink _) _ = s
      foldStatus s@(SpecialFile _) _ = s
      foldStatus s@(Empty _) _ = s
      foldStatus _ s@(Empty _) = s
      foldStatus s@(SameFile _ _) _ = s
      foldStatus _ s@(SameFile _ _) = s
      foldStatus s@(SameNameAndContents _ _) _ = s
      foldStatus _ s@(SameNameAndContents _ _) = s
      foldStatus s@(SameContents _ _) _ = s
      foldStatus _ s@(SameContents _ _) = s
      foldStatus s _ = s

showStatus :: Status -> IO ()
showStatus status = putStrLn (show status)

showCounts :: Status -> IO ()
showCounts status = return ()

showCommon :: Status -> IO ()
showCommon x@(SameContents _ _) = putStrLn . show $ x
showCommon x@(SameFile _ _) = putStrLn . show $ x
showCommon x@(SameNameAndContents _ _) = putStrLn . show $ x
showCommon _ = return ()

{-
data FileInfo = FileInfo { path :: FilePath
                         , status :: FileStatus
                         , handle :: Handle
                         , text :: B.ByteString }
-}
type FileInfo = (FilePath, FileStatus, Handle, B.ByteString)

-- |Find all the non-directory files within a directory.  Returns a
-- list of each file's size and path.
findPaths :: FilePath -> IO [(FileStatus, FilePath)]
findPaths top =
    getSymbolicLinkStatus top >>= flip find ""
    where
      find parentStatus path =
          -- Be very careful not to return a pair where one is a
          -- symlink to the other - you could lose both.
          do status <- getSymbolicLinkStatus (top ++ path)
             -- Don't enter any directory that is a mountpoint.  This
             -- will not detect a mount --bind of a file system onto
             -- itself; neither will mountpoint(1) by the way.
             let mountpoint = deviceID status /= deviceID parentStatus
             case (mountpoint, isDirectory status) of
               (True, _) -> return []
               (_, True) -> do names <- getDirectoryContents (top ++ path) >>= return . filter (not . flip elem [".", ".."])
                               let subdirs = map ((path ++ "/") ++) names
                               mapM (find status) subdirs >>= return . concat
               (_, False) -> return [(status, top ++ path)]

-- |Return a list of all pairs where one file is in the copy directory and one is in the original.
findCopies :: (Status -> IO ()) -> FilePath -> FilePath -> IO ()
findCopies f originalDir copyDir =
    do originals <- findPaths originalDir >>= return . groupFiles
       candidates <- findPaths copyDir >>= return . groupFiles
       findCopies' f originals candidates
    where
      -- Group files of equal size.
      groupFiles :: [(FileStatus, FilePath)] -> [(FileOffset, [(FileStatus, FilePath)])]
      groupFiles pairs = map unpair . group . sort $ pairs
      unpair :: [(FileStatus, FilePath)] -> (FileOffset, [(FileStatus, FilePath)])
      unpair [] = error $ "Internal error"
      unpair pairs@((status, _) : _) = (fileSize status, pairs)
      group = groupBy (\ a b -> fileSize (fst a) == fileSize (fst b))
      sort = sortBy (\ a b -> compare (fileSize (fst b)) (fileSize (fst a)))

findCopies' :: (Status -> IO ()) -> [(FileOffset, [(FileStatus, OrigPath)])] -> [(FileOffset, [(FileStatus, CopyPath)])] -> IO ()
findCopies' _ _ [] = return ()
findCopies' _ [] _ = return ()
findCopies' f allOriginals@((originalSize, originals) : nextOriginals) allCopies@((copySize, copies) : nextCopies) =
    case compare originalSize copySize of
      GT -> findCopies' f nextOriginals allCopies
      LT -> findCopies' f allOriginals nextCopies
      EQ -> compareFiles originals copies >>
            findCopies' f nextOriginals nextCopies
    where
      compareFiles originals copies =
          do let (originalStatus, originalPaths) = unzip originals
                 (copyStatus, copyPaths) = unzip copies
             originalHandles <- mapM open originalPaths
             originalText <- mapM read originalHandles
             copyHandles <- mapM open copyPaths
             copyText <- mapM read copyHandles
             let originalInfo = catMaybes . map zapMissing $ zip4 originalPaths originalStatus originalHandles originalText
             let copyInfo = catMaybes . map zapMissing $ zip4 copyPaths copyStatus copyHandles copyText
             let matches = map (getStatus originalInfo) $ copyInfo
             mapM_ f matches
             mapM_ (\ (_, _, handle, _) -> hClose handle) (originalInfo ++ copyInfo)
      open :: FilePath -> IO (Maybe Handle)
      open path = try (openBinaryFile path ReadMode) >>= either (return . const Nothing) (return . Just)
      read :: Maybe Handle -> IO (Maybe B.ByteString)
      read = maybe (return Nothing) (\ h -> B.hGetContents h >>= return . Just)
      zapMissing (a, b, Just c, Just d) = Just (a, b, c, d)
      zapMissing _ = Nothing
