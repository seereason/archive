module Backup
    ( BackupSpec(..)
    , VolumeSpec(..)
    , user
    , host
    , folder
    ) where

import		 Data.List
import		 URI
import		 Text.XHtml.Transitional hiding (archive)

-- |Specifies a list of directories to be backed up.
data BackupSpec
    = Backups { volumes :: [VolumeSpec]
              } deriving (Show, Read, Eq, Ord)

-- |Specifies a single directory to be backed up.
data VolumeSpec
    = Volume { index :: Int		-- An identifying integer
             , original :: URI		-- Location of the original files
             , copies :: URI		-- Location of the backup archives
             , enabled :: Bool		-- Whether to create new backups
             } deriving (Read, Eq, Ord)

instance Show VolumeSpec where
    show vol = ("Volume " ++
                "{ index = " ++ show (index vol) ++
                ", original = " ++ uriToHaskell (original vol) ++
                ", copies = " ++ uriToHaskell (copies vol) ++
                ", enabled = " ++ show (enabled vol) ++ " }")

uriToHaskell uri =
    ("URI " ++
     "{ uriScheme = " ++ show (uriScheme uri) ++
     ", uriAuthority = " ++ show (uriAuthority uri) ++
     ", uriPath = " ++ show (uriPath uri) ++
     ", uriQuery = " ++ show (uriQuery uri) ++
     ", uriFragment = " ++ show (uriFragment uri) ++ " }")

instance Ord URI where
    compare a b = compare (show a) (show b)

instance HTML BackupSpec where
    toHtml backups = concatHtml (map (tr . toHtml) (nub (volumes backups))) +++
                     newVolumeForm

instance HTML VolumeSpec where
    toHtml volume =
        td (stringToHtml (show (index volume))) +++
        td (textfield ("OriginalUser" ++ show (index volume)) ! [strAttr "value" (user (original volume)), intAttr "size" 15]) +++
        td (textfield ("OriginalHost" ++ show (index volume)) ! [strAttr "value" (host (original volume)), intAttr "size" 15]) +++
        td (textfield ("OriginalFolder" ++ show (index volume)) ! [strAttr "value" (folder (original volume)), strAttr "size" "30%"]) +++
        td (textfield ("ArchiveHost" ++ show (index volume)) ! [strAttr "value" (host (copies volume)), intAttr "size" 15]) +++
        td (textfield ("ArchiveFolder" ++ show (index volume)) ! [strAttr "value" (folder (copies volume)), strAttr "size" "30%"]) +++
        td (checkbox ("Enabled" ++ show (index volume)) "1"
            ! (if enabled volume then [intAttr "checked" 1] else [])) +++
        td (button (stringToHtml "Run") ! 
                       [strAttr "type" "submit",
                        strAttr "title" "Run",
                        strAttr "name" ("Run" ++ show (index volume)),
                        strAttr "value" "1"])

user uri = case uriAuthority uri of
             Nothing -> error ("Missing username: " ++ show uri)
             Just auth -> let user = unEscapeString (uriUserInfo auth) in
                          case splitAt (length user - 1) user of
                            (user', "@") -> user'
                            _ -> error ("Invalid username: " ++ user)
host uri = case uriAuthority uri of
             Nothing -> error ("Missing hostname: " ++ show uri)
             Just auth -> unEscapeString . uriRegName $ auth
folder = unEscapeString . uriPath

-- |Form to create a new volume.
newVolumeForm :: Html
newVolumeForm =
    tr (th (stringToHtml "New:") +++
        td (textfield "OriginalUser" ! [intAttr "size" 15]) ! valueAttr "OriginalUser" +++
        td (textfield "OriginalHost" ! [intAttr "size" 15]) ! valueAttr "OriginalHost" +++
        td (textfield "OriginalFolder") ! valueAttr "OriginalFolder" +++
        td (textfield "ArchiveHost" ! [intAttr "size" 15]) ! valueAttr "ArchiveHost" +++
        td (textfield "ArchiveFolder") ! valueAttr "ArchiveFolder" +++
        td (primHtmlChar "nbsp") +++
        td (primHtmlChar "nbsp")) +++
    tr (td (primHtmlChar "nbsp") +++
        th (button (stringToHtml "Update")
            ! [strAttr "type" "submit",
               strAttr "title" "Go",
               strAttr "name" "submit",
               strAttr "value" "1"]) ! [intAttr "colspan" 5] +++
        td (primHtmlChar "nbsp") ! [intAttr "colspan" 2])
    where
      valueAttr _ = [] -- maybe [] (\ s -> [strAttr "value" s]) (lookup name cgivars)
