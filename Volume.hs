module Volume where

import Ugly.Html.FORM
import Ugly.URI
import Text.XHtml.Transitional

-- |Specifies a single directory to be backed up.
data VolumeSpec
    = Volume { volumeId :: Maybe Int	-- An identifying integer
             , original :: URI		-- Location of the original files
             , copies :: URI		-- Location of the backup archives
             , enabled :: Bool		-- Whether to create new backups
             }
      deriving (Show, Read, Eq, Ord)

instance Element VolumeSpec where
    ident _ = "v"
    create = Volume {volumeId = Nothing, original = nullURI, copies = nullURI, enabled = False}
    update s _ _ = error $ "Undefined Volume update: " ++ s
    isIncomplete (Volume {volumeId = Nothing}) = True
    isIncomplete _ = False
    htmlElementShow nav volume@(Volume {volumeId = Nothing}) = return $ stringToHtml "Uninitialized"
    htmlElementShow nav volume@(Volume {volumeId = Just index}) =
        let gridclass = strAttr "class" (case (index `mod` 2) of
                                           0 -> "evengrid"
                                           _ -> "oddgrid") in
        return
        (tr (th (stringToHtml (show index)) ! [gridclass] +++
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
             td (maybe (primHtmlChar "nbsp") id nav) ! [intAttr "colspan" 2, gridclass]))
    htmlElementEdit = undefined

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
