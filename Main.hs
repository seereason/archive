-- |CGI command to manage backups.
module Main where

import		 Data.List
import		 Data.Maybe
import		 Control.Exception
import		 Control.Monad.Trans (liftIO)
import qualified Network.CGI as CGI
import		 URI
--import	 Network.URI
import		 System.Environment
import		 Text.XHtml.Transitional hiding (archive)
import		 System.Directory
import		 Backup
import		 Archive
import		 GHC.Read(readEither)
import		 System.IO
import		 Linspire.Unix.Process
import qualified Data.ByteString as B
import		 Data.Char

main :: IO ()
main = CGI.runCGI (CGI.handleErrors $ cgiMain)

cgiMain :: CGI.CGI CGI.CGIResult
cgiMain = 
    do path <- CGI.scriptName
       inputs <- CGI.getInputs
       moreInputs <- liftIO getEnvironment
       let cgivars = inputs ++ moreInputs
       do result <- application path cgivars
          case result of
            Left e ->
                CGI.output . show $ (h1 (stringToHtml "error") +++ 
                                     stringToHtml (show e) +++ br +++
                                     cgivarHtml cgivars)
            Right s -> CGI.output (show (s +++ cgivarHtml cgivars))
    where
      cgivarHtml cgivars =
          case {- lookup "show_cgi_vars" cgivars -} Just 1 of
            Just _ -> pre (stringToHtml (unlines (map show cgivars)))
            Nothing -> noHtml

-- |Compare the values in the configuration file with the values in the CGI
-- parameters.  If they are different, update the configuration file.  Then
-- perform any requested backups.  Finally, output the resulting HTML.
application :: FilePath -> [(String, String)] -> CGI.CGI (Either Html Html)
application _ cgivars = liftIO (updateConfig cgivars >>= useConfig cgivars)

mergeTry x = try x >>= return . either (Left . stringToHtml . ("mergeTry: " ++) . show) id

-- |Load the configuration from a file, update it using the values
-- passed from the CGI form, save it, and return it.
updateConfig :: [(String, String)] -> IO (Either Html BackupSpec)
updateConfig cgivars =
    mergeTry (checkConfigDirectory >>=
              checkConfigFile configPath >>=
              loadConfigFile cgivars >>=
              return . modifyConfig cgivars >>=
              saveConfig)

-- |Perform any operations requested, and then generate the new HTML.
useConfig :: [(String, String)] -> Either Html BackupSpec -> IO (Either Html Html)
useConfig _ (Left html) = return (Left html)
useConfig cgivars (Right backups) =
    do messages <- mapM runBackup (zip [1..] (volumes backups))
       return (Right (form (table (tr (th (stringToHtml "ID") ! [intAttr "rowspan" 2] +++
                                       th (stringToHtml "Original") ! [intAttr "colspan" 3] +++
                                       th (stringToHtml "Archives") ! [intAttr "colspan" 2] +++
                                       th (stringToHtml "Enabled") ! [intAttr "rowspan" 2] +++
                                       th (stringToHtml "Backup") ! [intAttr "rowspan" 2]) +++
                                   tr (th (stringToHtml "User") +++
                                       th (stringToHtml "Host") +++
                                       th (stringToHtml "Folder") ! [strAttr "size" "30%"] +++
                                       th (stringToHtml "Host") +++
                                       th (stringToHtml "Folder")) +++
                                   toHtml backups) ! [intAttr "border" 1, strAttr "width" "100%"])
                      ! [strAttr "method" "post"] +++ br +++ concatHtml (map showMessage messages)))
    where
      runBackup :: (Int, VolumeSpec) -> IO (Maybe (Either Html Html))
      runBackup (index, volume) =
          case lookup ("Run" ++ show index) cgivars of
            Just "1" ->
                do case (uriAuthority (original volume), uriAuthority (copies volume)) of
                     (_, Nothing) -> return . Just . Left . stringToHtml $ "Invalid destination URI: " ++ show (copies volume)
                     (Nothing, _) -> return . Just . Left . stringToHtml $ "Invalid original URI: " ++ show (original volume)
                     (Just orig, Just copy) ->
                            -- The archive commands needs to be run from the machine where the
                            -- copy will be created, so ssh there.
                         do let copyAddr = uriUserInfo copy ++ uriRegName copy
                            let origAddr = uriUserInfo orig ++ uriRegName orig
                            let cmd = ("set -x && " ++
                                       ssh ++ copyAddr ++ " '"
                                       ++ (archiveBin ++ " " ++
                                           "\"" ++ origAddr ++ ":" ++ uriPath (original volume) ++ "\"" ++ " " ++
                                           "\"" ++ uriPath (copies volume) ++ "\"") ++
                                       "'")
                            result <- lazyCommand cmd []
                            let output = stringToHtml . byteStringToString . B.concat . outputOnly $ result
                            case exitCodeOnly result of
                              (ExitSuccess : _) -> return . Just . Right . pre $ (stringToHtml cmd +++ br +++ stringToHtml " ->\n" +++ output)
                              x -> return . Just . Left . pre $ stringToHtml ("Failure: " ++ cmd ++ " -> " ++ show x ++ "\n") +++ output
{-
                do result <- archiveRemote [] (original volume) (copies volume)
                   let message = stringToHtml ("archive " ++ host (original volume) ++ ":" ++ folder (original volume) ++
                                               " " ++ host (copies volume) ++ ":" ++ folder (copies volume) ++ " -> ")
                   case result of
                     Left e -> return . Just . Left $ message +++ br +++ stringToHtml ("backup failed: " ++ show e) +++ br
                     Right r -> return . Just . Right $ message +++ stringToHtml (show r) +++ br
-}
            _ -> return Nothing
      showMessage Nothing = noHtml
      showMessage (Just (Left e)) = font e ! [strAttr "color" "red"]
      showMessage (Just (Right h)) = h +++ br

scp = "scp -o 'PreferredAuthentications hostbased,publickey' "
ssh = "ssh -o 'PreferredAuthentications hostbased,publickey' "
archiveBin = "/srv/backups/archive"
archiveTmp = "/tmp/archive"
archiveUser = "david"

{-
archiveRemote :: [Option] -> URI -> URI -> IO (Either IOError String)
archiveRemote options original backups =
-}

byteStringToString :: B.ByteString -> String
byteStringToString b = map (chr . fromInteger . toInteger) . B.unpack $ b

empty = Backups { volumes = [] }

fromJust' _ (Just x) = x
fromJust' s Nothing = error $ "fromJust: " ++ s

makeURI = fromJust . parseURI . escapeURIString (not . flip elem "{}") 

defaultUserInfoMap :: String -> String
defaultUserInfoMap uinf = user++newpass
    where
        (user,pass) = break (==':') uinf
        newpass     = if null pass || (pass == "@")
                                   || (pass == ":@")
                        then pass
                        else ":...@"

testDefaultUserInfoMap =
     [ defaultUserInfoMap ""                == ""
     , defaultUserInfoMap "@"               == "@"
     , defaultUserInfoMap "user@"           == "user@"
     , defaultUserInfoMap "user:@"          == "user:@"
     , defaultUserInfoMap "user:anonymous@" == "user:...@"
     , defaultUserInfoMap "user:pass@"      == "user:...@"
     , defaultUserInfoMap "user:pass"       == "user:...@"
     , defaultUserInfoMap "user:anonymous"  == "user:...@"
     ]

example = Backups 
          { volumes = [ Volume 
                        { index = 1
                        , original = makeURI "rsync://dsf@192.168.0.3/mnt/sdd2/audio"
                        , copies = makeURI "rsync://dsf@192.168.0.2/mnt/sdc2/backups/audio"
                        , enabled = False }
                      , Volume
                        { index = 2
                        , original = makeURI "rsync://dsf@192.168.0.3/mnt/sdd2/{archives}"
                        , copies = makeURI "rsync://dsf@192.168.0.2/mnt/sdc2/backups/{archives}"
                        , enabled = True }
                      , Volume
                        { index = 3
                        , original = makeURI "rsync://dsf@192.168.0.3/var/lib/geneweb"
                        , copies = makeURI "rsync://dsf@192.168.0.2/mnt/sdc2/backups/geneweb"
                        , enabled = True }
                      ]
          }

{-
eitherDo :: (a -> m (either e b)) -> Either e a -> m (Either e b)
eitherDo f (Left x) = return (Left x)
eitherDo f (Right x) = f x

eitherBool :: m Bool -> e -> a -> m (Either e a)
eitherBool flag e a = flag
-}

checkConfigDirectory :: IO (Either Html ())
checkConfigDirectory =
    doesDirectoryExist topDir >>=
    \ exists -> if exists then
                    (return . Right $ ()) else
                    (return . Left . stringToHtml $ "Missing directory on server: " ++ topDir)

checkConfigFile :: FilePath -> Either Html () -> IO (Either Html FilePath)
checkConfigFile _ (Left html) = return (Left html)
checkConfigFile path (Right ()) =
    doesFileExist path >>=
    \ exists -> if exists then
                    (return . Right $ path) else
                    (return . Left . stringToHtml $ "Configuration file does not exist: " ++ path)

loadConfigFile :: [(String, String)] -> Either Html FilePath -> IO (Either Html BackupSpec)
loadConfigFile cgivars path =
    case (lookup "useExample" cgivars, path) of
      (Just _, _) -> return (Right empty)
      (Nothing, Left html) -> return $ Left html
      (Nothing, Right path) ->
          try (readFile path) >>=
          either (return . Left . error . show) (return . Right) >>=
          return . readConfigFile

-- |Load the current backup configuration.  This needs to be robust,
-- if the default file won't load we need to try looking at backups or
-- perhaps other saved configurations, or else take other emergency
-- measures.
readConfigFile :: Either Html String -> Either Html BackupSpec
readConfigFile (Left html) = Left html
readConfigFile (Right text) = either (Left . stringToHtml) Right (readEither text)

-- |Examine the CGI input variables and decide whether and how
-- to modify the backup configuration.
modifyConfig :: [(String, String)] -> Either Html BackupSpec -> Either Html (BackupSpec, BackupSpec)
modifyConfig _ (Left html) = Left html
modifyConfig cgivars (Right oldBackups)
    | lookup "useExample" cgivars /= Nothing = Right (oldBackups, example)
    | otherwise = Right (oldBackups, createVolume (modifyVolumes oldBackups))
    where
      modifyVolumes backups =
          backups { volumes = map modifyVolume (zip [1..] (volumes backups)) }
      modifyVolume (index, volume) =
          case map (\ s -> lookup s cgivars) (map (++ (show index))
                                              ["OriginalUser", "OriginalHost", "OriginalFolder",
                                               "ArchiveHost", "ArchiveFolder"]) of
            [Just ou, Just oh, Just op, Just ah, Just ap] ->
                volume { original = URI { uriScheme = "rsync:"
                                        , uriAuthority = Just (URIAuth { uriUserInfo = (ou ++ "@")
                                                                       , uriRegName = oh
                                                                       , uriPort = "" })
                                        , uriPath = op
                                        , uriQuery = ""
                                        , uriFragment = "" }
                       , copies = URI { uriScheme = "rsync:"
                                      , uriAuthority = Just (URIAuth { uriUserInfo = (archiveUser ++ "@")
                                                                     , uriRegName = ah
                                                                     , uriPort = "" })
                                      , uriPath = ap
                                      , uriQuery = ""
                                      , uriFragment = "" }
                       , enabled = case lookup ("Enabled" ++ show index) cgivars of
                                     Just "1" -> True
                                     _ -> False }
            _ -> volume
      createVolume oldBackups =
          case map (\ s -> lookup s cgivars) ["OriginalHost", "OriginalUser", "OriginalFolder",
                                              "ArchiveHost", "ArchiveFolder"] of
            [Just "", Just "", Just "", Just "", Just ""] -> oldBackups
            [Just oh, Just ou, Just op, Just ah, Just ap] ->
                let newVolume = Volume { index = length (volumes oldBackups) + 1
                                       , original = URI { uriScheme = "rsync:"
                                                        , uriAuthority = Just (URIAuth { uriUserInfo = (ou ++ "@")
                                                                                       , uriRegName = oh
                                                                                       , uriPort = "" })
                                                        , uriPath = op
                                                        , uriQuery = ""
                                                        , uriFragment = "" }
                                       , copies = URI { uriScheme = "rsync:"
                                                      , uriAuthority = Just (URIAuth { uriUserInfo = (archiveUser ++ "@")
                                                                                     , uriRegName = ah
                                                                                     , uriPort = "" })
                                                      , uriPath = ap
                                                      , uriQuery = ""
                                                      , uriFragment = "" }
                                       , enabled = False } in
                oldBackups { volumes = volumes oldBackups ++ [newVolume] }
            _ -> oldBackups

-- |Write the updated configuration back to the configuration directory.
saveConfig :: Either Html (BackupSpec, BackupSpec) -> IO (Either Html BackupSpec)
saveConfig (Left html) = return (Left html)
saveConfig (Right (oldBackups, newBackups)) | oldBackups == newBackups =
                                                --return (Left (stringToHtml ("No changes:\n " ++ show oldBackups ++ "\n " ++ show newBackups)))
                                                return (Right oldBackups)
saveConfig (Right (_, newBackups)) =
    makeBackup configPath >>
    writeFile configPath (show newBackups) >>
    return (Right newBackups)
    where
      -- Some race conditions here - gaps between the time we check
      -- whether a file exists and remove or rename that file, a gap
      -- between the time we remove a file and the time we put anther
      -- file in its place.
      makeBackup path =
          do let backup = path ++ "~"
             exists <- doesFileExist path
             case exists of
               False -> return ()
               True -> do exists' <- doesFileExist backup
                          case exists' of
                            False -> return ()
                            True -> removeFile backup
                          renameFile path backup

topURI = "/backups"
topDir = "/var/www" ++ topURI
configPath = topDir ++ "/backups.hs"
