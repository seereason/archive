module Target where

import Archive
import Control.Monad
import Data.Monoid
import Extra.HughesPJ
import qualified Text.PrettyPrint.HughesPJ as D
import System.IO.Error
-- import Text.PrettyPrint.HughesPJ

import Extra.Help hiding (options)

-- * Data Type

data Target
    = Target { prettyName :: String -- ^ name used to refer to this target
             , src :: [FilePath] -- ^ list of locations where the data to mirror can be found, order by preference
             , dest :: FilePath  -- ^ directory which holds all the snapshots
             , config :: Config -- ^ the configuration specification used for this target
             , options :: [Option] -- ^ options that should always be used with this target
             }

-- * Pretty Print

showTargets :: [Target] -> Elements
showTargets = foldr (<>) mempty . map showTarget 

showTarget :: Target -> Elements
showTarget (Target prettyName srcs dest _ options) =
    (text (prettyName ++ ":")) <> rs Nothing (tp Nothing (text "src: ") <> mconcat (map (\src -> text src <> br) srcs) <>
                                              tp Nothing (text "dest:") <> text dest <> br <>
                                              tp Nothing (text "options:") <> showOptions options)
    where
      showOptions :: [Option] -> Elements
      showOptions opts = mconcat (map showOption opts)
      showOption :: Option -> Elements
      showOption (Rsync str) = text "Rsync " <> cw <> (text str) <> p <> br

-- * Archive Target

archiveTargets :: [Option] -> [Target] -> IO [(Target, Either IOError (Maybe UpdateResult))]
archiveTargets options targets = liftM (zip targets) $ mapM (archiveTarget options) targets

archiveTarget :: [Option] -> Target -> IO (Either IOError (Maybe UpdateResult))
archiveTarget _ (Target prettyName [] _ _ _) = 
    return $ Left (userError $ "target " ++ prettyName ++ " does not include any sources")
archiveTarget extraOptions (Target prettyName (src:_) dest config options) =
    try (archive config (options ++ extraOptions) src dest)

ppResults :: [(Target, Either IOError (Maybe UpdateResult))] -> D.Doc
ppResults = D.vcat . map ppResult

ppResult :: (Target, Either IOError (Maybe UpdateResult)) -> D.Doc
ppResult (t, (Left e)) = D.text (prettyName t) D.<> D.colon D.<+> D.text (show e)
ppResult (t, Right Nothing) = D.text (prettyName t) D.<+> D.text "ok."
ppResult (t, Right (Just Changes)) = D.text (prettyName t) D.<+> D.text "ok. (updated)"
ppResult (t, Right (Just NoChanges)) = D.text (prettyName t) D.<+> D.text "ok. (no changes)"


-- *  Old

{-
showTargets :: [Target] -> Doc
showTargets targets = vcat (punctuate (text "" $+$ text "") (map showTarget targets))

showTarget :: Target -> Doc
showTarget (Target prettyName srcs dest _) = 
    (text prettyName) $$ nest 4 (text "source" $+$ (vcat (map text srcs)))

-}
