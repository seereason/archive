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
    = Volume { volumeId :: Maybe Int	-- An identifying integer
             , original :: URI		-- Location of the original files
             , copies :: URI		-- Location of the backup archives
             , enabled :: Bool		-- Whether to create new backups
             }
      deriving (Read, Eq, Ord)

instance Show VolumeSpec where
    show vol = ("Volume " ++
                "{ volumeId = " ++ show (volumeId vol) ++
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
    toHtml backups =
        form
        (table
         (tr (th (font (stringToHtml "Backups") ! [intAttr "size" 5]) ! [intAttr "colspan" 7]) +++
          tr (th (stringToHtml "ID") +++
              th (primHtmlChar "nbsp") +++
              th (stringToHtml "User") +++
              th (stringToHtml "Host") +++
              th (stringToHtml "Folder") +++
              th (stringToHtml "Enabled") +++
              th (primHtmlChar "nbsp")) +++
          concatHtml (map toHtml (volumes backups)) +++
          newVolumeForm (length (volumes backups) + 1))
         ! [-- intAttr "border" 1,
            strAttr "align" "center",
            strAttr "border" "0",
            strAttr "cellpadding" "0",
            strAttr "cellspacing" "0",
            strAttr "style" "width: 250px",
            strAttr "width" "100%"])
        ! [strAttr "method" "post"]
        where
          newVolumeForm :: Int -> Html
          newVolumeForm vol =
              let gridclass = strAttr "class" (case (vol `mod` 2) of
                                                 0 -> "evengrid"
                                                 _ -> "oddgrid") in
              tr (th (stringToHtml "New:") ! [gridclass] +++
                  th (stringToHtml "Original:") ! [gridclass]  +++
                  td (textfield "OriginalUser" ! [intAttr "size" 15, gridclass]) ! (valueAttr "OriginalUser" ++ [gridclass])  +++
                  td (textfield "OriginalHost" ! [intAttr "size" 15, gridclass]) ! (valueAttr "OriginalHost" ++ [gridclass]) +++
                  td (textfield "OriginalFolder" ! [gridclass]) ! (valueAttr "OriginalFolder" ++ [intAttr "size" 30, gridclass])  +++
                  td (primHtmlChar "nbsp") ! [intAttr "colspan" 2, gridclass]) +++
              tr (td (primHtmlChar "nbsp") ! [gridclass] +++
                  th (stringToHtml "Archive:") ! [gridclass] +++
                  td (primHtmlChar "nbsp") ! [gridclass] +++
                  td (textfield "ArchiveHost" ! [intAttr "size" 15, gridclass]) ! (valueAttr "ArchiveHost" ++ [gridclass]) +++
                  td (textfield "ArchiveFolder" ! [gridclass]) ! (valueAttr "ArchiveFolder" ++ [gridclass]) +++
                  td (primHtmlChar "nbsp") ! [gridclass] +++
                  td (primHtmlChar "nbsp") ! [gridclass]) +++
              tr (td (primHtmlChar "nbsp") +++
                  th (button (stringToHtml "Update")
                      ! [strAttr "type" "submit",
                         strAttr "title" "Go",
                         strAttr "name" "submit",
                         strAttr "value" "1"]) ! [intAttr "colspan" 5] +++
                  td (primHtmlChar "nbsp") ! [intAttr "colspan" 2])

          valueAttr _ = [] -- maybe [] (\ s -> [strAttr "value" s]) (lookup name cgivars)

instance HTML VolumeSpec where
    toHtml (Volume { volumeId = Nothing }) = noHtml
    toHtml (volume@Volume { volumeId = Just index }) = 
        let gridclass = strAttr "class" (case (index `mod` 2) of
                                           0 -> "evengrid"
                                           _ -> "oddgrid") in
        tr (th (stringToHtml (show index)) ! [gridclass] +++
            th (stringToHtml "Original:") ! [gridclass] +++
            td (textfield ("OriginalUser" ++ show index) ! [strAttr "value" (user (original volume)),
                                                            intAttr "size" 15, gridclass]) ! [gridclass] +++
            td (textfield ("OriginalHost" ++ show index) ! [strAttr "value" (host (original volume)),
                                                            intAttr "size" 15, gridclass]) ! [gridclass] +++
            td (textfield ("OriginalFolder" ++ show index) ! [strAttr "value" (folder (original volume)),
                                                              intAttr "size" 30, gridclass]) ! [gridclass] +++
            th (checkbox ("Enabled" ++ show index) "1" ! (if enabled volume then [intAttr "checked" 1] else [])) ! [gridclass] +++
            th (button (stringToHtml "Run") ! 
                [strAttr "type" "submit",
                 strAttr "title" "Run",
                 strAttr "name" ("Run" ++ show index),
                 strAttr "value" "1"]) ! [gridclass]) +++
        tr (td (primHtmlChar "nbsp") ! [gridclass] +++
            th (stringToHtml "Archive:") ! [gridclass] +++
            td (primHtmlChar "nbsp") ! [gridclass] +++
            td (textfield ("ArchiveHost" ++ show index) ! [strAttr "value" (host (copies volume)),
                                                           intAttr "size" 15, gridclass]) ! [gridclass] +++
            td (textfield ("ArchiveFolder" ++ show index) ! [strAttr "value" (folder (copies volume)),
                                                             intAttr "size" 30, gridclass]) ! [gridclass] +++
            td (primHtmlChar "nbsp") ! [intAttr "colspan" 2, gridclass])

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

