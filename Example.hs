module Example where

import Backup
import Ugly.URI

example =
    Backups {volumes = [Volume { volumeId = Just 1,
                                 original = URI { uriScheme = "rsync:", uriAuthority = Just (URIAuth {uriUserInfo = "david@", uriRegName = "192.168.0.3", uriPort = ""}), uriPath = "/mnt/sdd2/audio", uriQuery = "", uriFragment = "" },
                                 copies = URI { uriScheme = "rsync:", uriAuthority = Just (URIAuth {uriUserInfo = "david@", uriRegName = "192.168.0.2", uriPort = ""}), uriPath = "/mnt/sdc2/backups/audio", uriQuery = "", uriFragment = "" },
                                 enabled = False },
                        Volume { volumeId = Just 2,
                                 original = URI { uriScheme = "rsync:",
                                                  uriAuthority = Just (URIAuth {uriUserInfo = "david@", uriRegName = "192.168.0.3", uriPort = ""}),
                                                  uriPath = "/mnt/hda2/{archives}",
                                                  uriQuery = "",
                                                  uriFragment = "" },
                                 copies = URI { uriScheme = "rsync:",
                                                uriAuthority = Just (URIAuth {uriUserInfo = "david@", uriRegName = "192.168.0.2", uriPort = ""}),
                                                uriPath = "/mnt/sdc2/backups/{archives}",
                                                uriQuery = "",
                                                uriFragment = "" },
                                 enabled = True },
                        Volume { volumeId = Just 3,
                                 original = URI { uriScheme = "rsync:",
                                                  uriAuthority = Just (URIAuth {uriUserInfo = "root@", uriRegName = "192.168.0.3", uriPort = ""}),
                                                  uriPath = "/var/lib/geneweb",
                                                  uriQuery = "",
                                                  uriFragment = "" },
                                 copies = URI { uriScheme = "rsync:",
                                                uriAuthority = Just (URIAuth {uriUserInfo = "david@", uriRegName = "192.168.0.2", uriPort = ""}),
                                                uriPath = "/mnt/sdc2/backups/geneweb",
                                                uriQuery = "",
                                                uriFragment = "" },
                                 enabled = True },
                        Volume { volumeId = Just 4,
                                 original = URI { uriScheme = "rsync:",
                                                  uriAuthority = Just (URIAuth {uriUserInfo = "root@", uriRegName = "192.168.0.3", uriPort = ""}),
                                                  uriPath = "/mnt/hda2/images",
                                                  uriQuery = "",
                                                  uriFragment = "" },
                                 copies = URI { uriScheme = "rsync:",
                                                uriAuthority = Just (URIAuth {uriUserInfo = "david@", uriRegName = "192.168.0.2", uriPort = ""}),
                                                uriPath = "/mnt/sdc2/backups/images",
                                                uriQuery = "",
                                                uriFragment = "" },
                                 enabled = False },
                        Volume { volumeId = Just 5,
                                 original = URI { uriScheme = "rsync:",
                                                  uriAuthority = Just (URIAuth {uriUserInfo = "root@", uriRegName = "192.168.0.2", uriPort = ""}),
                                                  uriPath = "/mnt/sdc2/david",
                                                  uriQuery = "",
                                                  uriFragment = "" },
                                 copies = URI { uriScheme = "rsync:",
                                                uriAuthority = Just (URIAuth {uriUserInfo = "david@", uriRegName = "192.168.0.3", uriPort = ""}),
                                                uriPath = "/mnt/sdd2/backups/david@dsf",
                                                uriQuery = "",
                                                uriFragment = "" },
                                 enabled = False },
                        Volume { volumeId = Just 6,
                                 original = URI { uriScheme = "rsync:",
                                                  uriAuthority = Just (URIAuth {uriUserInfo = "root@",
                                                                                uriRegName = "192.168.0.2",
                                                                                uriPort = ""}),
                                                  uriPath = "/mnt/freespire/home/david",
                                                  uriQuery = "",
                                                  uriFragment = "" },
                                 copies = URI { uriScheme = "rsync:",
                                                uriAuthority = Just (URIAuth {uriUserInfo = "david@",
                                                                              uriRegName = "192.168.0.3",
                                                                              uriPort = ""}),
                                                uriPath = "/mnt/sdd2/backups/david2@dsf",
                                                uriQuery = "",
                                                uriFragment = "" },
                                 enabled = False },
                        Volume { volumeId = Just 7,
                                 original = URI { uriScheme = "rsync:",
                                                  uriAuthority = Just (URIAuth {uriUserInfo = "root@",
                                                                                uriRegName = "192.168.0.2",
                                                                                uriPort = ""}),
                                                  uriPath = "/mnt/sda2/dsf",
                                                  uriQuery = "",
                                                  uriFragment = "" },
                                 copies = URI { uriScheme = "rsync:",
                                                uriAuthority = Just (URIAuth {uriUserInfo = "david@",
                                                                              uriRegName = "192.168.0.3",
                                                                              uriPort = ""}),
                                                uriPath = "/mnt/sdd2/backups/david3@dsf",
                                                uriQuery = "",
                                                uriFragment = "" },
                                 enabled = False },
                        Volume { volumeId = Just 8,
                                 original = URI { uriScheme = "rsync:",
                                                  uriAuthority = Just (URIAuth {uriUserInfo = "root@",
                                                                                uriRegName = "192.168.0.3",
                                                                                uriPort = ""}),
                                                  uriPath = "/mnt/sdd2/www",
                                                  uriQuery = "",
                                                  uriFragment = "" },
                                 copies = URI { uriScheme = "rsync:",
                                                uriAuthority = Just (URIAuth {uriUserInfo = "david@",
                                                                              uriRegName = "192.168.0.2",
                                                                              uriPort = ""}),
                                                uriPath = "/mnt/sdc2/backups/www",
                                                uriQuery = "",
                                                uriFragment = "" },
                                 enabled = False }]}
{-
example = Backups
          {volumes = [Volume { volumeId = Just 1
                             , original = URI { uriScheme = "rsync:"
                                              , uriAuthority = Just (URIAuth {uriUserInfo = "david@"
                                                                             , uriRegName = "192.168.0.3"
                                                                             , uriPort = ""})
                                              , uriPath = "/mnt/sdd2/audio"
                                              , uriQuery = ""
                                              , uriFragment = "" }
                             , copies = URI { uriScheme = "rsync:"
                                            , uriAuthority = Just (URIAuth {uriUserInfo = "david@"
                                                                           , uriRegName = "192.168.0.2"
                                                                           , uriPort = ""})
                                            , uriPath = "/mnt/sdc2/backups/audio"
                                            , uriQuery = ""
                                            , uriFragment = "" }
                             , enabled = False
                             , orphans = [] }
                     ,Volume { volumeId = Just 2
                             , original = URI { uriScheme = "rsync:"
                                              , uriAuthority = Just (URIAuth {uriUserInfo = "david@"
                                                                             , uriRegName = "192.168.0.3"
                                                                             , uriPort = ""})
                                              , uriPath = "/mnt/hda2/{archives}"
                                              , uriQuery = ""
                                              , uriFragment = "" }
                             , copies = URI { uriScheme = "rsync:"
                                            , uriAuthority = Just (URIAuth {uriUserInfo = "david@"
                                                                           , uriRegName = "192.168.0.2"
                                                                           , uriPort = ""})
                                            , uriPath = "/mnt/sdc2/backups/{archives}"
                                            , uriQuery = ""
                                            , uriFragment = "" }
                             , enabled = True
                             , orphans = [] }
                     ,Volume { volumeId = Just 3
                             , original = URI { uriScheme = "rsync:"
                                              , uriAuthority = Just (URIAuth {uriUserInfo = "root@"
                                                                             , uriRegName = "192.168.0.3"
                                                                             , uriPort = ""})
                                              , uriPath = "/var/lib/geneweb"
                                              , uriQuery = ""
                                              , uriFragment = "" }
                             , copies = URI { uriScheme = "rsync:"
                                            , uriAuthority = Just (URIAuth {uriUserInfo = "david@"
                                                                           , uriRegName = "192.168.0.2"
                                                                           , uriPort = ""})
                                            , uriPath = "/mnt/sdc2/backups/geneweb"
                                            , uriQuery = ""
                                            , uriFragment = "" }
                             , enabled = True
                             , orphans = [] }
                     ,Volume { volumeId = Just 4
                             , original = URI { uriScheme = "rsync:"
                                              , uriAuthority = Just (URIAuth {uriUserInfo = "root@"
                                                                             , uriRegName = "192.168.0.3"
                                                                             , uriPort = ""})
                                              , uriPath = "/mnt/hda2/images"
                                              , uriQuery = ""
                                              , uriFragment = "" }
                             , copies = URI { uriScheme = "rsync:"
                                            , uriAuthority = Just (URIAuth {uriUserInfo = "david@"
                                                                           , uriRegName = "192.168.0.2"
                                                                           , uriPort = ""})
                                            , uriPath = "/mnt/sdc2/backups/images"
                                            , uriQuery = ""
                                            , uriFragment = "" }
                             , enabled = False
                             , orphans = [] }
                     ,Volume { volumeId = Just 5
                             , original = URI { uriScheme = "rsync:"
                                              , uriAuthority = Just (URIAuth {uriUserInfo = "root@"
                                                                             , uriRegName = "192.168.0.2"
                                                                             , uriPort = ""})
                                              , uriPath = "/mnt/sdc2/david"
                                              , uriQuery = ""
                                              , uriFragment = "" }
                             , copies = URI { uriScheme = "rsync:"
                                            , uriAuthority = Just (URIAuth {uriUserInfo = "david@"
                                                                           , uriRegName = "192.168.0.3"
                                                                           , uriPort = ""})
                                            , uriPath = "/mnt/sdd2/backups/david@dsf"
                                            , uriQuery = ""
                                            , uriFragment = "" }
                             , enabled = False
                             , orphans = [Orphan { orphanVolume = Just 5
                                                 , orphanId = Nothing
                                                 , orphanURI = URI { uriScheme = "rsync:"
                                                                   , uriAuthority = Just (URIAuth {uriUserInfo = "root@", uriRegName = "192.168.0.2", uriPort = ""})
                                                                   , uriPath = "/mnt/freespire/home/david"
                                                                   , uriQuery = ""
                                                                   , uriFragment = ""}}]}
                     ]
          }
-}
