module Main where

import System.Environment (withArgs)
import System.Archive.UpdateMirror

-- * Update List of Targets Here

mytargets :: [Target]
mytargets =
    [ RsyncTarget { prettyName = "ubuntu"
                  , src = [ "rsync://ubuntu.cs.utah.edu/ubuntu"
                          , "rsync://mirror.anl.gov/ubuntu" 
                          ]
                  , dest = "/srv/mirrors/ubuntu.com"
                  , config = genericConfig "ubuntu" "%Y-%m-%d_%H:%M:%S"
                  , options = (exclude defaultExcludeArches [] True) ++ [Rsync "--progress", Rsync "--stats"]
                  }
    , AptTarget   { prettyName = "linuxmint"
                  , src = [ "http://www.linuxmint.com/repository/" ]
                  , dest = "/tmp/linuxmint.com/"
                  , config = genericConfig "linuxmint" "%Y-%m-%d_%H:%M:%S"
                  , options = []
                  , dists = [ ("daryna", ["i386"])
                            ]
                  }
    , AptTarget   { prettyName = "test-apt"
                  , src = [ "http://fattushis/addons/ubuntu/CNRUbuntuExtra/" ]
                  , dest = "/tmp/test-mirror/"
                  , config = genericConfig "test" "%Y-%m-%d_%H:%M:%S"
                  , options = []
                  , dists = [ ("gutsy-extra", ["i386"])
                            ]
                  }
    , RsyncTarget { prettyName = "test-rsync"
                  , src = [ "rsync://fattushis/srv/addons/ubuntu/CNRUbuntuExtra/" ]
                  , dest = "/tmp/test-mirror/"
                  , config = genericConfig "test" "%Y-%m-%d_%H:%M:%S"
                  , options = (exclude defaultExcludeArches [] True) ++ [Rsync "--progress", Rsync "--stats"]
                  }
    ]

defaultExcludeArches :: [[Char]]
defaultExcludeArches = 
    [ "alpha"
    , "ia64"
    , "arm"
    , "hppa"
    , "hurd-i386"
    , "m68k"
    , "mips"
    , "mipsel"
    , "powerpc"
    , "s390"
    , "sparc"
    ]

ubuntuDists :: String -> [String]
ubuntuDists dist = map (\suffix -> dist ++ suffix) ["","-backports", "-proposed", "-security", "-updates"]


-- note: currently excluding dists only excludes the index files, not the files in the pool
exclude :: [String] -> [String] -> Bool -> [Option]
exclude arches dists excludeOther = concatMap excludeArch arches ++ map excludeDist dists ++ (if excludeOther then otherExcludes else [])
 where
   excludeArch arch =
       [ Rsync $ "--exclude=*"++arch++".deb"
       , Rsync $ "--exclude=binary-" ++ arch
       , Rsync $ "--exclude=disks-" ++ arch
       , Rsync $ "--exclude=Contents-"++arch++".gz"
       , Rsync $ "--exclude=Contents-"++arch++".diff"
       ]
   excludeDist dist = Rsync $ "--exclude=dists/" ++ dist
   otherExcludes =
       [ Rsync $ "--exclude=debian-installer"
       , Rsync $ "--exclude=debian-installer-*"
       , Rsync $ "--exclude=installer-*"
       , Rsync $ "--exclude=daily-installer-*"
       , Rsync $ "--exclude=project"
       , Rsync $ "--exclude=indices"
       ]
-- * generic main function

main :: IO ()
main = updateMirrorMain mytargets

