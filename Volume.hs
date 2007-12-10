module Volume where

import Ugly.Html.FORM
import Ugly.URI
import Text.XHtml.Transitional
import Control.Monad.Reader

-- |Specifies a single directory to be backed up.
data VolumeSpec
    = Volume { -- volumeId :: Maybe Int	-- An identifying integer
               original :: URI		-- Location of the original files
             , copies :: URI		-- Location of the backup archives
             , enabled :: Bool		-- Whether to create new backups
             }
      deriving (Show, Read, Eq, Ord)

instance Element VolumeSpec where
    ident _ = "v"
    create = Volume {original = nullURI, copies = nullURI, enabled = False}
    isIncomplete vol = original vol == nullURI || copies vol == nullURI
    update "run" _ volume = volume
    update "Enabled" v volume = volume {enabled = if v == "1" then True else (error $ "Unexpected value of enabled: " ++ v)}
    update "OriginalUser" v volume = volume {original = setURIUser v (original volume)}
    update "OriginalHost" v volume = volume {original = setURIHost v (original volume)}
    update "OriginalFolder" v volume = volume {original = setURIFolder v (original volume)}
    update "ArchiveHost" v volume = volume {copies = setURIHost v (copies volume)}
    update "ArchiveFolder" v volume = volume {original = setURIFolder v (copies volume)}
    update s _ _ = error $ "Undefined Volume update: " ++ s
    htmlElementShow nav volume =
        do info <- ask
           let index = infoIndex info
               gridclass = strAttr "class" (if index `mod` 2 == 0 then "evengrid" else "oddgrid")
               origUser = maybe noHtml stringToHtml (user (original volume))
               origHost = maybe noHtml stringToHtml (host (original volume))
               origFolder = stringToHtml (folder (original volume))
               archiveHost = maybe noHtml stringToHtml (host (copies volume))
               archiveFolder = stringToHtml (folder (copies volume))
           let (html :: Html)
                   = (tr (th (stringToHtml (show index)) ! [gridclass] +++
                          th (stringToHtml "Original:") ! [gridclass] +++
                          td origUser ! [gridclass] +++
                          td origHost ! [gridclass] +++
                          td origFolder ! [gridclass] +++
                          th noHtml ! [gridclass] +++
                          th (button (stringToHtml "Run") ! 
                              [strAttr "type" "submit",
                               strAttr "title" "Run",
                               strAttr "name" (prefix info ++ "run"),
                               strAttr "value" "1"]) ! [gridclass]) +++
                      tr (td (primHtmlChar "nbsp") ! [gridclass] +++
                          th (stringToHtml "Archive:") ! [gridclass] +++
                          td (primHtmlChar "nbsp") ! [gridclass] +++
                          td archiveHost ! [gridclass] +++
                          td archiveFolder ! [gridclass] +++
                          td (maybe (primHtmlChar "nbsp") id nav)
                          ! [intAttr "colspan" 2, gridclass]))
           return html
    htmlElementEdit nav volume =
        do info <- ask
           let index = infoIndex info
           let gridclass = strAttr "class" (if index `mod` 2 == 0 then "evengrid" else "oddgrid")
           origUser <- formTextfield "OriginalUser" (user (original volume)) [intAttr "size" 15]
           origHost <- formTextfield "OriginalHost" (host (original volume)) [intAttr "size" 15]
           origFolder <- formTextfield "OriginalFolder" (Just (folder (original volume))) [intAttr "size" 30]
           archiveHost <- formTextfield "ArchiveHost" (host (copies volume)) [intAttr "size" 15]
           archiveFolder <- formTextfield "ArchiveFolder" (Just (folder (copies volume))) [intAttr "size" 30]
           enabledCheckbox <- formCheckbox "Enabled" (enabled volume) []
           let (html :: Html)
                   = (tr (th (stringToHtml (show index)) ! [gridclass] +++
                          th (stringToHtml "Original:") ! [gridclass] +++
                          td origUser ! [gridclass] +++
                          td origHost ! [gridclass] +++
                          td origFolder ! [gridclass] +++
                          th enabledCheckbox ! [gridclass] +++
                          th (button (stringToHtml "Run") ! 
                              [strAttr "type" "submit",
                               strAttr "title" "Run",
                               strAttr "name" (prefix info ++ "run"),
                               strAttr "value" "1"]) ! [gridclass]) +++
                      tr (td (primHtmlChar "nbsp") ! [gridclass] +++
                          th (stringToHtml "Archive:") ! [gridclass] +++
                          td (primHtmlChar "nbsp") ! [gridclass] +++
                          td archiveHost ! [gridclass] +++
                          td archiveFolder ! [gridclass] +++
                          td (maybe (primHtmlChar "nbsp") id nav)
                          ! [intAttr "colspan" 2, gridclass]))
           return html
    htmlList _vols elems = return $ concatHtml elems

user uri = case uriAuthority uri of
             Nothing -> Nothing
             Just auth -> let user = unEscapeString (uriUserInfo auth) in
                          case splitAt (length user - 1) user of
                            (user', "@") -> Just user'
                            _ -> error ("Invalid username: " ++ user)
host uri = case uriAuthority uri of
             Nothing -> Nothing
             Just auth -> Just . unEscapeString . uriRegName $ auth

folder = unEscapeString . uriPath

setURIUser user uri@(URI {uriAuthority = Just auth}) = uri {uriAuthority = Just (auth {uriUserInfo = user ++ "@"})}
setURIUser user uri@(URI {uriAuthority = Nothing}) = setURIUser user (uri {uriAuthority = Just nullURIAuth})
setURIHost host uri@(URI {uriAuthority = Just auth}) = uri {uriAuthority = Just (auth {uriRegName = host})}
setURIHost host uri@(URI {uriAuthority = Nothing}) = setURIHost host (uri {uriAuthority = Just nullURIAuth})
setURIFolder folder uri = uri {uriPath = folder}
nullURIAuth = URIAuth {uriUserInfo = "", uriRegName = "", uriPort = ""}