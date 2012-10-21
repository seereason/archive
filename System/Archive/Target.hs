module System.Archive.Target where

import Control.Applicative.Error (Failing(Success, Failure))
import Control.Exception (try)
import Control.Monad
import Data.Monoid (Monoid(..))
import Data.List
import qualified Text.PrettyPrint.HughesPJ as D
import System.Archive.Archive
-- import System.IO.Error hiding (try)
-- import System.Unix.QIO (quieter)
-- import Text.PrettyPrint.HughesPJ

import Text.Help hiding (options)

-- * Data Type

data Target
    = RsyncTarget { prettyName :: String -- ^ name used to refer to this target
                  , src :: [FilePath] -- ^ list of locations where the data to mirror can be found, order by preference
                  , dest :: FilePath  -- ^ directory which holds all the snapshots
                  , config :: Config -- ^ the configuration specification used for this target
                  , options :: [Option] -- ^ options that should always be used with this target
                  }
    | AptTarget  { prettyName :: String -- ^ name used to refer to this target
                 , src :: [FilePath] -- ^ list of locations where the data to mirror can be found, order by preference
                 , dest :: FilePath  -- ^ directory which holds all the snapshots
                 , config :: Config -- ^ the configuration specification used for this target
                 , options :: [Option] -- ^ options that should always be used with this target
                 , dists :: [(String, [String])] -- ^ [(dist, arches)]
                 }

-- * Pretty Print

showTargets :: [Target] -> Elements
showTargets = foldr (<>) mempty . map showTarget 

showTarget :: Target -> Elements
showTarget (RsyncTarget prettyName srcs dest _ options) =
    (text (prettyName ++ ":")) <> rs Nothing (tp Nothing (text "src: ") <> mconcat (map (\src -> text src <> br) srcs) <>
                                              tp Nothing (text "dest:") <> text dest <> br <>
                                              tp Nothing (text "options:") <> showOptions options)
    where
      showOptions :: [Option] -> Elements
      showOptions opts = mconcat (map showOption opts)
      showOption :: Option -> Elements
      showOption (Rsync str) = text "Rsync " <> cw <> (text str) <> p <> br
      showOption NoUpdateSymlink = text "NoUpdateSymlink" <> br
showTarget (AptTarget prettyName srcs dest _ options dists) =
    (text (prettyName ++ ":")) <> rs Nothing (tp Nothing (text "src: ") <> mconcat (map (\src -> text src <> br) srcs) <>
                                              tp Nothing (text "dest:") <> text dest <> br <>
                                              tp Nothing (text "options:") <> showOptions options <>
                                              tp Nothing (text "dists:") <> showDists dists)
    
    where
      showOptions :: [Option] -> Elements
      showOptions opts = mconcat (map showOption opts)
      showOption :: Option -> Elements
      showOption (Rsync str) = text "Rsync " <> cw <> (text str) <> p <> br
      showOption NoUpdateSymlink = text "NoUpdateSymlink" <> br
      showDists :: [(String, [String])] -> Elements
      showDists dists = mconcat (map showDist dists)
      showDist :: (String, [String]) -> Elements
      showDist (dist, arches) = text (dist ++ " ") <> showArches arches
      showArches :: [String] -> Elements
      showArches arches = text "[" <> mconcat (intersperse (text ", ") (map text arches)) <> text "]"

-- * Archive Target

archiveTargets :: [Option] -> [Target] -> IO [(Target, Either IOError (Failing UpdateResult))]
archiveTargets options targets =
    -- quieter (\ n -> n + 2 - length (filter (== (Rsync "-v")) options)) $
    liftM (zip targets) $ mapM (archiveTarget options) targets

-- TODO: create repository -> current symlink
archiveTarget :: [Option] -> Target -> IO (Either IOError (Failing UpdateResult))
archiveTarget _ target | null (src target) =
    return $ Left (userError $ "target " ++ (prettyName target) ++ " does not include any sources")
archiveTarget extraOptions (RsyncTarget _prettyName (src:_) dest config options) =
    try (archive config (options ++ extraOptions) src dest [])
archiveTarget extraOptions (AptTarget   _prettyName (src:_) dest config options dists) =
    try (archive config (options ++ extraOptions) src dest dists)

ppResults :: [(Target, Either IOError (Failing UpdateResult))] -> D.Doc
ppResults = D.vcat . map ppResult

ppResult :: (Target, Either IOError (Failing UpdateResult)) -> D.Doc
ppResult (t, (Left e)) = D.text (prettyName t) D.<> D.colon D.<+> D.text (show e)
ppResult (t, Right (Failure msgs)) = D.text (prettyName t) D.<+> D.text (unlines msgs)
ppResult (t, Right (Success Unknown)) = D.text (prettyName t) D.<+> D.text "ok."
ppResult (t, Right (Success Changes)) = D.text (prettyName t) D.<+> D.text "ok. (updated)"
ppResult (t, Right (Success NoChanges)) = D.text (prettyName t) D.<+> D.text "ok. (no changes)"


-- *  Old

{-
showTargets :: [Target] -> Doc
showTargets targets = vcat (punctuate (text "" $+$ text "") (map showTarget targets))

showTarget :: Target -> Doc
showTarget (Target prettyName srcs dest _) = 
    (text prettyName) $$ nest 4 (text "source" $+$ (vcat (map text srcs)))

-}
