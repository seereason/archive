module Main where

import UpdateMirror

-- * Update List of Targets Here

mytargets :: [Target]
mytargets =
    [ Target { prettyName = "ubuntu"
             , src = [ "http://archive.ubuntu.com/ubuntu"
                     , "http://mirror.anl.gov/ubuntu"
                     ]
             , dest = "/mnt/aoe/ubuntu"
             , config = genericConfig "ubuntu" "%Y-%m-%d"
             , options = [ Rsync "--exclude Contents-i386.gz"
                         , Rsync "--exclude Contents-amd64.gz"
                         ]
             }
    , Target { prettyName = "linuxmint"
             , src = ["http://linuxmint.com/linuxmint"]
             , dest = "/mnt/aoe/linuxmint"
             , config = genericConfig "linuxmint" "%Y-%m-%d"
             , options = [ Rsync "--exclude Contents-i386.gz"
                         , Rsync "--exclude Contents-amd64.gz"
                         ]
             }
    ]

-- * generic main function

main :: IO ()
main = updateMirrorMain mytargets

