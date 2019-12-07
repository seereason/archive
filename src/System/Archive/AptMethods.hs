module System.Archive.AptMethods
    (updateViaAptMethods
    )
    where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Debian.Apt.Methods
import Debian.Apt.Index hiding (update)
import Debian.Control.ByteString
import Debian.Mirror
import System.FilePath
import System.Directory
import System.Exit
import System.Posix.Files
import System.Unix.FilePath (dirName)
import Network.URI

test =
    fetchOrLink [] (fromJust $ parseURI "http://archive.ubuntu.com/ubuntu/dists/gutsy/Release") "/tmp/curr" (Just "/tmp/prev") 


myCallbacks :: FetchCallbacks
myCallbacks = 
    cliFetchCallbacks { uriDoneCB = 
                            \uri size lastModified resumePoint filename hashes imsHit -> 
                                putStrLn $ uriToString' uri ++ " downloaded" ++ maybe "." (" to " ++) filename
                      }

uriDoneCBLink existingFP = \uri size lastModified resumePoint filename hashes imsHit -> 
            if imsHit
             then do createLink existingFP (fromJust filename)
                     putStrLn $ uriToString' uri ++ " hardlinked."
             else putStrLn $ uriToString' uri ++ " downloaded" ++ maybe "." (" to " ++) filename

uriToString' uri = uriToString id uri ""
-- |if the target files already exists, we will always break any
-- hardlinks before attempting to fetch. So, even if the file is not
-- updated.
fetchOrLink :: [ConfigItem] -> URI -> FilePath -> Maybe FilePath -> IO Bool
fetchOrLink configItems remotePath localPath mExistingFP =
    do flip when (breakLink localPath) =<< fileExist localPath
       case mExistingFP of
         Nothing -> 
             fetch myCallbacks configItems remotePath localPath =<< getLastModified localPath
         (Just existingFP) -> 
             fetch (myCallbacks { uriDoneCB = uriDoneCBLink existingFP }) configItems remotePath localPath =<< getLastModified existingFP
       
-- * To Be Moved 

-- break a hardlink
breakLink :: FilePath -> IO ()
breakLink fp =
    do status <- getSymbolicLinkStatus fp
       when (linkCount status > 1) (copyFile fp fp)

{-

The apt methods do not break hard-links. Therefore we need to
explicitly break the hard-links for it.

We can divide the stuff we need to download into two categories:
 - files we only have a timestamp for (Release files, etc)
 - stuff we have md5sums for

If we have the md5sum some, then we can check the files on the disk
already, and link the ones we have. The ones we do not have, we can
download via the methods.

Since we do not have a good way to automatically enumerate the dists,
the list of dists to mirror will have to be an explicit list.

Steps:
-----

1. Download/hardlink the Release files for each dist
2. Parse Release files and generate list of all required files
3. Attempt to hardlink to local files
4. Attempt to download remaining files
5. Rename download directory if everything successfully retrieved

How to recover from aborted download:

1. The bulk of the data is the debs, not the release files 2. If we
have the ability to find old copies in more than one location, then we
can just add the previous .in-progress directory to the list of places
to look for .debs. However, we also need to validate the size/md5sum
of files in the .in-progress directory, since it is likely one of them
was only download halfway. However, our hardlink algorithm will
probably already have the option to check size and md5sum.

What should we do if a Release File is missing? How about throw an
exception? 

-}

updateViaAptMethods :: [FilePath]  -- ^ path to previous snapshot directories
       -> URI -- ^ base path to remote repository
       -> FilePath -- ^ base path to local directory
       -> [(String, [String])] -- ^ list of (dist, arches) to download
       -> IO (ExitCode)
updateViaAptMethods prevBasePaths remoteURI basePath dists =
    do -- fetch the relevant files from the 'dists' directory
       mapM_ fetchControlFiles dists
       -- the local dists directory now contains all the control files
       -- parse them and find out what pool files we need
       poolFiles <- liftM concat $ mapM (createPoolFileList basePath) dists
       missing <- liftM catMaybes $ mapM (fetchPoolFile prevBasePaths basePath) poolFiles
       if null missing
          then return (ExitSuccess)
          else return (ExitFailure 1)
    where
      prevBasePath = listToMaybe prevBasePaths
      fetchControlFiles (dist, arches) =
          do let distPath = "dists" </> dist
                 releaseFP = distPath </> "Release"
             ensureParentDirectoryExists (basePath </> releaseFP)
             fetchOrLink [] (remoteURI { uriPath = (uriPath remoteURI) </> releaseFP }) (basePath </> releaseFP) (fmap (</> releaseFP) prevBasePath)
             release <- mustParseControlFromFile (basePath </> releaseFP)
             let indexFiles = indexesInRelease (archFilter arches) release
                 fetch = (\fp -> ensureParentDirectoryExists (basePath </> distPath </> fp) >>
                                 (fetchOrLink [] (remoteURI { uriPath = (uriPath remoteURI) </> distPath </> fp })
                                                  (basePath </> distPath </> fp)
                                                  (fmap (</> (distPath </> fp)) prevBasePath)))
             -- download index files found in Release
             mapM_ (\ (_,_,fp) -> fetch fp) indexFiles
             -- download other files not found in Release
             mapM_ fetch (["Release.gpg"] ++ map (\arch -> "Contents-" ++ arch ++ ".gz") arches)
      fetchPoolFile :: [FilePath] -> FilePath -> FileTuple -> IO (Maybe FileTuple)
      fetchPoolFile prevBasePaths basePath ft@(checksum, size, filename) =
          do doneAlready <- fileExist (basePath </> filename) -- TODO: check size/md5sum
             if doneAlready
                then do putStrLn $ filename ++ " already exists in pool."
                        return Nothing
                else do
                  ensureParentDirectoryExists (basePath </> filename) -- silly, but this gets us a log message, so..
                  res <- fetchOrLink [] (remoteURI { uriPath = (uriPath remoteURI) </> filename }) (basePath </> filename) (fmap (</> filename) prevBasePath)
                  if res
                     then return Nothing
                     else return (Just ft)
             where 
               prevBasePath = listToMaybe prevBasePaths
      filename (_,_,fp) = fp
      ensureParentDirectoryExists filepath =
          do let dir = dirName filepath
             putStrLn $ "Ensuring " ++ dir ++ " exists."
             createDirectoryIfMissing True dir
      mustParseControlFromFile fp =
          do r <- parseControlFromFile fp
             case r of
               (Left e) -> error (show e)
               (Right c) -> return c
      createPoolFileList basePath (dist, arches) =
          do let distDir = (basePath </> "dists" </> dist)
             release <- mustParseControlFromFile (distDir </> "Release")
             let indexFiles = indexesInRelease (archFilter arches) release
             binaryIndexes <- findIndexes distDir "Packages" indexFiles
             binaryFiles   <- liftM concat $ mapM (makePackageFileListIO distDir) binaryIndexes
             sourceIndexes <- findIndexes distDir "Sources" indexFiles
             sourceFiles   <- liftM concat $ mapM (makeSourceFileListIO  distDir) sourceIndexes
             return (nubOn filename  (binaryFiles ++ sourceFiles))
      nubOn :: (Ord b) => (a -> b) -> [a] -> [a]
      nubOn selector list = map head $ groupBy ((==) `on` selector) $ sortBy (compare `on` selector) list
{-
    let releaseFPs = map ((\dist -> "dists" </> dist </> "Release") . fst) dists -- hrm, we need to do this on a dist by dist basis
        prevBasePath = listToMaybe prevBasePaths
    in do -- create dist directories
          mapM_ ((\dir -> putStrLn ("Ensuring " ++ dir ++ " exists.") >> createDirectoryIfMissing True dir) . (basePath </>) . dirName) releaseFPs
          -- fetch Release files
          mapM_ (\fp -> fetchOrLink [] (remoteURI { uriPath = (uriPath remoteURI) </> fp }) (basePath </> fp) (fmap (</> fp) prevBasePath) >>= 
                 (flip unless) (error $ "Failed to fetch: " ++ fp)) releaseFPs
          -- read the release files and get a list of the other indexes
          releases <- mapM mustParseControlFromFile (map (basePath </>) releaseFPs)
          let indexFiles = concatMap (indexesInRelease (const True)) releases
          -- add checks for size/checksums
          mapM_ (\fp -> fetchOrLink [] (remoteURI { uriPath = (uriPath remoteURI) </> "dists" </> fp }) (basePath </> "dists" </> fp) (fmap (</> ("dists" </> fp)) prevBasePath)) (map (\ (_,_,fp) -> fp) indexFiles)
          -- (distFiles, _) <- liftM (\l -> (concatMap fst l, concatMap snd l)) $ mapM (\ (dist, arches) -> makeDistFileList (archFilter arches) basePath dist) dists
          -- return ()
          -- get all the indexes
          -- mapM_ (
          -- mapM_ print distFiles
          -- mapM_ print (poolFiles)
          -- print (length distFiles)
    where
      mustParseControlFromFile fp =
          do r <- parseControlFromFile fp
             case r of
               (Left e) -> error (show e)
               (Right c) -> return c
                  

-}
          
          

-- | apply monadic filter to list until failure is encountered
-- return True if no failures
-- return False if failed
allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM f [] = return True
allM f (h:t) =
    do b <- f h
       case b of
         False -> return False
         True -> allM f t
