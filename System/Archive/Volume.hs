module System.Archive.Volume where

import Ugly.HtmlForm
import Ugly.Encoding.Octets
import Ugly.URI
import Text.XHtml.Transitional
import Control.Monad.Reader
import System.Unix.Process
import qualified Data.ByteString.Char8 as B
-- Debug
import System.IO


-- |Specifies a single directory to be backed up.
data VolumeSpec
    = Volume { -- volumeId :: Maybe Int	-- An identifying integer
               origUUID :: Maybe String	-- UUID of original volume
             , copyUUID :: Maybe String	-- UUID of volume holding the archives
             , original :: URI		-- Location of the original files
             , copies :: URI		-- Location of the backup archives
             , enabled :: Bool		-- Whether to create new backups
             }
      deriving (Show, Read, Eq, Ord)

instance Element VolumeSpec where
    ident _ = "v"
    create = Volume { original = nullURI, origUUID = Nothing
                    , copies = nullURI, copyUUID = Nothing, enabled = False}
    isIncomplete vol = original vol == nullURI || copies vol == nullURI
    updateM a v x = return $ update a v x
    update "run" _ volume = volume
    update "Enabled" v volume = volume {enabled = if unpack v == "1" then True else (error $ "Unexpected value of enabled: " ++ unpack v)}
    --update "OriginalUser" v volume = volume {original = setURIUser (unpack v) (original volume)}
    update "OriginalHost" v volume = volume {original = setURIHost (unpack v) (original volume)}
    update "OriginalFolder" v volume = volume {original = setURIFolder (unpack v) (original volume)}
    update "ArchiveHost" v volume = volume {copies = setURIHost (unpack v) (copies volume)}
    update "ArchiveFolder" v volume = volume {original = setURIFolder (unpack v) (copies volume)}
    update s _ _ = error $ "Undefined Volume update: " ++ s
    finish volume =
        updateOrigUUID volume >>= updateCopyUUID
        where
          -- Compute and save the volume's UUID
          updateOrigUUID :: VolumeSpec -> FORM VolumeSpec
          updateOrigUUID (Volume {origUUID = Nothing}) =
              liftIO (computeUUID (original volume)) >>= \ uuid -> return (volume {origUUID = Just uuid})
	  -- Compute and compare the volume's UUID
          updateOrigUUID (Volume {origUUID = Just saved}) =
              liftIO (computeUUID (original volume)) >>= \ uuid -> if uuid == saved
                                                                   then return volume
                                                                   else error ("UUID mismatch on " ++ show (original volume) ++
                                                                               ": expected " ++ saved ++ ", actual: " ++ uuid)
          -- Compute and save the copy's UUID
          updateCopyUUID :: VolumeSpec -> FORM VolumeSpec
          updateCopyUUID (Volume {copyUUID = Nothing}) =
              liftIO (computeUUID (copies volume)) >>= \ uuid -> return (volume {copyUUID = Just uuid})
	  -- Compute and compare the copy's UUID
          updateCopyUUID (Volume {copyUUID = Just saved}) =
              liftIO (computeUUID (copies volume)) >>= \ uuid -> if uuid == saved
                                                                 then return volume
                                                                 else error ("UUID mismatch on " ++ show (copies volume) ++
                                                                             ": expected " ++ saved ++ ", actual: " ++ uuid)
    htmlElement nav volume False =
        do info <- ask
           let index = infoIndex info
               gridclass = strAttr "class" (if index `mod` 2 == 0 then "evengrid" else "oddgrid")
               origHost = maybe noHtml stringToHtml (host (original volume))
               origFolder = stringToHtml (folder (original volume))
               archiveHost = maybe noHtml stringToHtml (host (copies volume))
               archiveFolder = stringToHtml (folder (copies volume))
           let (html :: Html)
                   = (tr (th (stringToHtml (show index)) ! [gridclass] +++
                          th (stringToHtml "Original:") ! [gridclass] +++
                          td (maybe noHtml stringToHtml (origUUID volume)) ! [gridclass] +++
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
                          td (maybe noHtml stringToHtml (copyUUID volume)) ! [gridclass] +++
                          td archiveHost ! [gridclass] +++
                          td archiveFolder ! [gridclass] +++
                          td (maybe (primHtmlChar "nbsp") id nav)
                          ! [intAttr "colspan" 2, gridclass]))
           return html
    htmlElement nav volume True =
        do info <- ask
           let index = infoIndex info
           let gridclass = strAttr "class" (if index `mod` 2 == 0 then "evengrid" else "oddgrid")
           origHost <- formTextfield "OriginalHost" (host (original volume)) [intAttr "size" 15]
           origFolder <- formTextfield "OriginalFolder" (Just (folder (original volume))) [intAttr "size" 30]
           archiveHost <- formTextfield "ArchiveHost" (host (copies volume)) [intAttr "size" 15]
           archiveFolder <- formTextfield "ArchiveFolder" (Just (folder (copies volume))) [intAttr "size" 30]
           enabledCheckbox <- formCheckbox "Enabled" (enabled volume) []
           let (html :: Html)
                   = (tr (th (stringToHtml (show index)) ! [gridclass] +++
                          th (stringToHtml "Original:") ! [gridclass] +++
                          td (maybe noHtml stringToHtml (origUUID volume)) ! [gridclass] +++
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
                          td (maybe noHtml stringToHtml (copyUUID volume)) ! [gridclass] +++
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

computeUUID :: URI -> IO String
computeUUID uri =
    lazyCommand cmd [] >>= return . head . lines . B.unpack . B.concat . stdoutOnly
    where
      cmd = case uriAuthority uri of
              Just auth -> 
                  ("ssh root@" ++ uriRegName auth
                   ++ " \""
                   -- ++ "set -x; "
                   ++ "stat --format '%d' '" ++ uriPath uri ++ "'"
                   ++ " | awk '{printf (\\\"%d:%d\\\\n\\\", (\\$1 / 256), (\\$1 % 256))}'"
                   ++ " | while read N; do find /sys/block -name dev "
                   ++ " | while read F; do dirname \\$F "
                   ++ " | while read D; do basename \\$D "
                   ++ " | while read B; do cat \\$F "
                   ++ " | while read X; do "
                   ++ "if [ \\$N = \\$X ]; then echo \\$B; fi"
                   ++ "; done"
                   ++ "; done"
                   ++ "; done"
                   ++ "; done"
                   ++ "; done"
                   ++ "\"")
              Nothing -> error $ "Invalid uri: " ++ show uri
