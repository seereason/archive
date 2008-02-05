#!/usr/bin/runhaskell

module Main where

import Control.Monad(unless)
import System.Cmd
import System.Directory
import System.Posix.User
import System.Posix.Files
import System.Environment
import System.Exit
import System.IO

main =
    generatePublicKey >>=
    either (return . Left) getDest >>=
    either (return . Left) testAccess >>=
    either (return . Left) openAccess >>=
    either (error . show) (const . exitWith $ ExitSuccess)

-- |Make sure there is a public key for the local account
generatePublicKey :: IO (Either String (FilePath, Int))
generatePublicKey =
    do let port = 22
       user <- getEnv "USER"
       home <- getUserEntryForName user >>= return . homeDirectory
       let keypath = home ++ "/.ssh/id_rsa.pub"
       exists <- doesFileExist keypath
       case exists of
         True -> return . Right $ (keypath, port)
         False ->
             do hPutStrLn stderr $ "generatePublicKey " ++ show (keypath, port)
                result <- system cmd
                case result of
                  ExitSuccess -> return . Right $ (keypath, port)
                  ExitFailure n -> return . Left $ "Failure: " ++ cmd ++ " -> " ++ show n
    where cmd = "yes '' | ssh-keygen -t rsa; fi"

-- |Get the destination account info from the command line
getDest :: (String, Int) -> IO (Either String (FilePath, Int, String))
getDest (keypath, port) =
    getArgs >>= checkArgs
    where checkArgs [dest] = return . Right $ (keypath, port, dest)
          checkArgs args = return . Left $ "Unexpected arguments: " ++ show args ++ "\nUsage: sshexport user@host"

-- |See if we already have access to the account
testAccess :: (FilePath, Int, String) -> IO (Either String (Maybe (FilePath, Int, String)))
testAccess (keypath, port, dest) =
    do hPutStrLn stderr $ "testAccess " ++ show (keypath, port, dest)
       let cmd = sshTestCmd port dest
       result <- system cmd
       case result of
         ExitSuccess -> return . Right $ Nothing
         ExitFailure _ -> return . Right . Just $ (keypath, port, dest)
    where
      sshTestCmd port dest =
          ("ssh -o 'PreferredAuthentications hostbased,publickey' -p " ++
           show port ++ " " ++ dest ++ " pwd && " ++
           "echo 'Authentication succeeded.' && exit 0")

-- |Try to set up the keys so we have access to the account
openAccess :: Maybe (FilePath, Int, String) -> IO (Either String ())
openAccess Nothing = return . Right $ ()
openAccess (Just (keypath, port, dest)) =
    do hPutStrLn stderr $ "openAccess " ++ show (keypath, port, dest)
       let cmd = sshOpenCmd keypath port dest
       result <- system cmd
       case result of
         ExitSuccess -> exitWith ExitSuccess
         ExitFailure n -> error $ "Failure: " ++ cmd ++ " -> " ++ show n
    where
      sshOpenCmd keypath port dest =
          "cat " ++ keypath ++ " | " ++ "ssh -p " ++ show port ++ " " ++ dest ++ " '" ++ sshOpenRemoteCmd ++ "'"
      sshOpenRemoteCmd =
          ("chmod g-w . && " ++				-- Ssh will not work if the permissions aren't just so
           "chmod o-rwx . && " ++
           "mkdir -p .ssh && " ++
           "chmod 700 .ssh && " ++
           "cat >> .ssh/authorized_keys2 && " ++	-- Add the key to the authorized key list
           "chmod 600 .ssh/authorized_keys2")

{-
main =
    do args <- getArgs
       case args of
         [dest] ->
             do let port = 22
                user <- getEnv "USER"
                home <- getUserEntryForName user >>= return . homeDirectory
                let keypath = home ++ "/.ssh/id_rsa.pub"
                exists <- doesFileExist keypath
                let cmd0 = "yes '' | ssh-keygen -t rsa; fi"
                result0 <-
                    case exists of
                      True -> return ExitSuccess
                      False -> system cmd0
                case result0 of
                  ExitFailure n -> error $ "Failure: " ++ cmd0 ++ " -> " ++ show n
                  ExitSuccess ->
                      do let cmd1 = sshTestCmd port dest
                         result1 <- system cmd1
                         case result1 of
                           ExitSuccess -> exitWith ExitSuccess
                           ExitFailure n ->
                               do let cmd2 = sshOpenCmd keypath port dest
                                  result2 <- system cmd2
                                  case result2 of
                                    ExitFailure n -> error $ "Failure: " ++ cmd2 ++ " -> " ++ show n
                                    ExitSuccess -> exitWith ExitSuccess
-}
