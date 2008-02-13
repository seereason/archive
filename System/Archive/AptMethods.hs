import Debian.Apt.Methods
import Ugly.URI
import Data.Maybe

test =
    do lm <- getLastModified "/tmp/linked.html"
       fetch myCallbacks [] (fromJust $ parseURI "http://archive.ubuntu.com/ubuntu/dists/gutsy/Release") "/tmp/test.html" lm

myCallbacks = 
 cliFetchCallbacks { uriDoneCB = \uri size lastModified resumePoint filename hashes imsHit -> 
                                 if imsHit 
                                    then putStrLn $ uriToString' uri ++ " hardlinked." 
                                    else putStrLn $ uriToString' uri ++ " downloaded." }

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

-}
{-
update :: [FilePath]  -- ^ path to previous snapshots
       -> URI -- ^ base path to remote repository
       -> [(String, String)] -- ^ list of (dist, arch) to download
       -> IO ()
update snapshots baseURI dists =
    -}