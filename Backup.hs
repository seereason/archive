-- |Data type representing something to back up, where it gets
-- backed up to, how often to back it up, etc.
module Backup
    ( BackupSpec(..)
    , VolumeSpec(..)
    , user
    , host
    , folder
    ) where

import Control.Arrow hiding ((+++))
import Data.Char
import Data.List
import Ugly.Html.FORM
import Ugly.Html.Style
import Ugly.Encoding.Octets
import Text.XHtml.Transitional hiding (archive)
import Volume
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B

-- |Specifies a list of directories to be backed up.
data BackupSpec
    = Backups { volumes :: [VolumeSpec]
              , status :: Html
              } deriving (Show, Read, Eq)

instance Form BackupSpec where
    defaultForm = create
    formUpdate = element
    htmlHead _ = return $ myStyle +++ thetitle (stringToHtml "Backups")
    htmlInputs = htmlElementShow Nothing

instance Element BackupSpec where
    ident _ = "b"
    element prefix command spec = 
        spec {volumes = traverse prefix command (volumes spec)}
    create = Backups {volumes = [], status = noHtml}
    update s _ _ = error $ "Undefined Backup update: " ++ s
    htmlElementShow nav spec =
        do (topnav, content) <- htmlSubList (volumes spec)
           script <- formURI
           let nav' = maybe (maybe noHtml id topnav) id nav
           extra <- debug
           let backups =
                   (table
                    (tr (-- It is important to have a submit button at the
                         -- top of the form because otherwise hitting the
                         -- enter key on a textarea input causes submits
                         -- that appears to be going to another control,
                         -- like the top navigation insert.
                         th (submit "submit" "update") +++
                         th nav' +++
                         th (formLink script (font (stringToHtml "Backups") 
                                              ! [intAttr "size" 5]) (Nothing :: Maybe VolumeSpec)) ! [intAttr "colspan" 4] +++
                         th (formLink script (stringToHtml "Edit") (Just (undefined :: VolumeSpec)))) +++
                     tr (th (stringToHtml "ID") +++
                         th (primHtmlChar "nbsp") +++
                         th (stringToHtml "User") +++
                         th (stringToHtml "Host") +++
                         th (stringToHtml "Folder") +++
                         th (stringToHtml "Enabled") +++
                         th (primHtmlChar "nbsp")) +++
                     content)
                    ! [-- intAttr "border" 1,
                       strAttr "align" "center",
                       strAttr "border" "0",
                       strAttr "cellpadding" "0",
                       strAttr "cellspacing" "0",
                       strAttr "style" "width: 250px",
                       strAttr "width" "100%"] +++
                    br +++ status spec +++ br +++ extra)
           return backups
        where
           debug = do info <- ask
                      return $ case lookup "debug" (cgivars info) of
                                 Nothing -> noHtml
                                 _ -> concatHtml (intersperse br (map (stringToHtml . show . second unpack) (cgivars info)))
           unpack (Octets s) = map (chr . fromInteger . toInteger) . B.unpack $ s
    htmlElementEdit = undefined
    htmlList = undefined

instance List BackupSpec VolumeSpec where
    getElements spec = volumes spec
    setElements spec xs = spec {volumes = xs}
