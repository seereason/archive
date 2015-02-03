{-# LANGUAGE OverloadedStrings #-}

import Control.Category ((.))
import Data.Text (pack)
import Debian.AutoBuilder.Details.CabalInfo (seereasonDefaults)
import Debian.Debianize
import Debian.Policy (SourceFormat(Native3), StandardsVersion(StandardsVersion))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))
import Prelude hiding ((.))

main :: IO ()
main = newFlags >>= newCabalInfo >>= evalCabalT (debianize customize >> liftCabal writeDebianization)

customize :: CabalT IO ()
customize = seereasonDefaults >> (utilsPackageNameBase . debInfo) ~= Just "archive"
