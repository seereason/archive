{-# LANGUAGE OverloadedStrings #-}
import Data.Text (pack)
import Debian.Debianize (changelog, conflicts, control, debianize, depends, executable, inputChangeLog, installTo,
                         postInst, postRm, provides, replaces, seereasonDefaultAtoms, sourceFormat, utilsPackageName)
import Debian.Debianize.ControlFile (homepage, standardsVersion)
import Debian.Debianize.Types (InstallFile(InstallFile, destDir, destName, execName, sourceDir), Top(Top))
import Debian.Policy (SourceFormat(Native3), StandardsVersion(StandardsVersion))
import Debian.Relation (BinPkgName(BinPkgName), Relation(Rel))

main :: IO ()
main = debianize (Top ".") (utilsPackageName (BinPkgName "archive")) seereasonDefaultAtoms
