-- |Data type representing something to back up, where it gets
-- backed up to, how often to back it up, etc.
module Backup
    ( BackupSpec(..)
    , VolumeSpec(..)
    , user
    , host
    , folder
    ) where

import Data.List
import Ugly.Html.FORM
import Ugly.Html.Style
import Ugly.URI
import Text.XHtml.Transitional hiding (archive)
import Volume

-- |Specifies a list of directories to be backed up.
data BackupSpec
    = Backups { volumes :: [VolumeSpec]
              } deriving (Show, Read, Eq, Ord)

instance Form BackupSpec where
    defaultForm = create
    formUpdate = element
    htmlHead _ = return myStyle
    htmlInputs = htmlElementShow Nothing

instance Element BackupSpec where
    ident _ = "b"
    element prefix command spec = 
        spec {volumes = traverse prefix command (volumes spec)}
    create = Backups {volumes = []}
    update s _ _ = error $ "Undefined Backup update: " ++ s
    htmlElementShow nav spec =
        do (topnav, content) <- htmlSubList spec (volumes spec)
           uri <- formURI
           let nav' = maybe topnav Just nav
           return content
    htmlElementEdit = undefined

instance List BackupSpec VolumeSpec where
    getElements spec = volumes spec
    setElements spec xs = spec {volumes = xs}
    htmlList spec vols elems =
        do uri <- formURI
           return (table
                   (tr (th (font (stringToHtml "Backups") ! [intAttr "size" 5]) ! [intAttr "colspan" 7]) +++
                    tr (th (stringToHtml "ID") +++
                        th (primHtmlChar "nbsp") +++
                        th (stringToHtml "User") +++
                        th (stringToHtml "Host") +++
                        th (stringToHtml "Folder") +++
                        th (stringToHtml "Enabled") +++
                        th (primHtmlChar "nbsp")) +++
                    concatHtml elems +++
                    newVolumeForm (length (volumes spec) + 1) uri)
                   ! [-- intAttr "border" 1,
                      strAttr "align" "center",
                      strAttr "border" "0",
                      strAttr "cellpadding" "0",
                      strAttr "cellspacing" "0",
                      strAttr "style" "width: 250px",
                      strAttr "width" "100%"])
        where
          newVolumeForm :: Int -> URI -> Html
          newVolumeForm vol uri =
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
                         strAttr "value" "1"] +++
                      formLink uri (stringToHtml "Edit Volumes") (Just (undefined :: VolumeSpec)))
                  ! [intAttr "colspan" 5] +++
                  td (primHtmlChar "nbsp") ! [intAttr "colspan" 2])

          valueAttr _ = [] -- maybe [] (\ s -> [strAttr "value" s]) (lookup name cgivars)
