{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Control.Concurrent   (threadDelay)
import Control.Monad.Extra  (findM, whenM)
import Control.Monad.State  (MonadState, StateT, put, runStateT)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.List            (isPrefixOf, sortBy)
import Data.Time            (defaultTimeLocale, formatTime, getCurrentTime,
                             utcToLocalZonedTime)
import System.Directory     (createDirectoryLink, doesDirectoryExist,
                             doesPathExist, getHomeDirectory,
                             getSymbolicLinkTarget, listDirectory,
                             pathIsSymbolicLink, removeDirectoryLink,
                             withCurrentDirectory)
import System.FilePath      (isRelative, takeFileName, (</>))
import System.Process       (callCommand)
import Text.Printf          (printf)

type Source = FilePath
type Dest = FilePath
type BackupItem = (Source, Dest)

class Monad c => BackupConfigWriter c where
  dest   :: Dest   -> c ()
  backup :: Source -> c ()

newtype BackupConfigImpl a
  = BackupConfigImpl (StateT Dest (Writer [Source]) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Dest
    , MonadWriter [Source]
    )

replaceHome path
  = if "~" `isPrefixOf` path
    then do
      home <- getHomeDirectory
      return $ home ++ tail path
    else return path

getBackupItems :: BackupConfigImpl () -> IO [BackupItem]
getBackupItems (BackupConfigImpl s) = do
  dest2 <- replaceHome dest
  return . map (\src -> (src, dest2)) $ srcs
  where (((), dest), srcs) = runWriter . runStateT s $ ""

instance BackupConfigWriter BackupConfigImpl where
  dest = put
  backup src = tell [src]

currentBackup :: BackupItem -> FilePath
currentBackup (src, dest) = dest </> (src ++ ".current")

data ValidationResult
  = ValidResult BackupItem
  | CurrentLinkDoesntExist BackupItem FilePath
  | BackupDoesntExist BackupItem
  deriving Show

findBackup :: BackupItem -> IO (Maybe FilePath)
findBackup (src, dest) = do
  files <- listDirectory dest
  let srcPrefixFiles = filter ((src ++ ".") `isPrefixOf`) $ files
  if null srcPrefixFiles
    then return Nothing
    else do
      let backups = sortBy (flip compare) $ srcPrefixFiles
      findM
        ( \backup -> do
            let absoluteBackup = dest </> backup
            doesDirectoryExist absoluteBackup >>= \case
              True -> return True
              False -> do
                printf "Backup is not a directory: %s\n" absoluteBackup
                return False
        )
        backups >>= \case
          Just goodBackup -> return . Just $ dest </> goodBackup
          Nothing -> return Nothing

validateItem :: BackupItem -> IO ValidationResult
validateItem item@(src, dest) = do
  whenM (not <$> doesPathExist src) $ do
    fail $ "Source to backup doesn't exists: " ++ src
  whenM (not <$> doesDirectoryExist dest) $ do
    fail $ "Backup destination directory doesn't exists: " ++ dest
  let current = currentBackup item
  doesPathExist current >>= \case
    False -> findBackup item >>= \case
      Just backup -> do
        printf
          ( "Current backup link destination doesn't exist: %s, "
            ++ "but backup found: %s\n"
          )
          current backup
        return $ CurrentLinkDoesntExist item backup
      Nothing -> do
        printf
          ( "Current backup link destination doesn't exist: %s, "
            ++ "and no backup found\n"
          )
          current
        return $ BackupDoesntExist item
    True -> do
      whenM (not <$> pathIsSymbolicLink current) $ do
        fail $ "Current backup link destination is not a symlink: " ++ current
      currentTarget <- getSymbolicLinkTarget current
      let absoluteCurrentTarget = if isRelative currentTarget
            then dest </> currentTarget
            else currentTarget
      whenM (not <$> doesDirectoryExist absoluteCurrentTarget) $ do
        fail
          $ printf
            "Current backup link destination is not a directory: %s -> %s"
            current absoluteCurrentTarget
      return $ ValidResult item

validateItems :: [BackupItem] -> IO [ValidationResult]
validateItems items = mapM validateItem items

backupTimestamp :: IO String
backupTimestamp = do
  time <- utcToLocalZonedTime =<< getCurrentTime
  return . formatTime defaultTimeLocale "%Y%m%d_%H%M" $ time

tenSecondsMs :: Int
tenSecondsMs = 10*10^6

backupDirectoryName item@(src, dest) = do
  timestamp <- backupTimestamp
  dirName <- replaceHome $ dest </> (src ++ "." ++ timestamp)
  doesPathExist dirName >>= \case
    True -> do
      printf "Path '%s' already exists, sleeping for 10 seconds\n" dirName
      threadDelay tenSecondsMs
      backupDirectoryName item
    False -> return dirName

sh :: String -> IO ()
sh cmd = do
  printf "Executing: %s\n" cmd
  callCommand cmd

updateCurrentLink backupDir item = do
  removeDirectoryLink (currentBackup item)
  createCurrentLink backupDir item

createCurrentLink backupDir (src, dest)
  = withCurrentDirectory dest $ do
      let target = takeFileName backupDir
      let link   = src ++ ".current"
      printf "Creating the %s -> %s symlink in %s\n" link target dest
      createDirectoryLink target link

backupItem (ValidResult item@(src, _)) = do
  let current = currentBackup item
  backupDir <- backupDirectoryName item
  sh $ printf "rsync -a --link-dest=%s %s %s" current src backupDir
  updateCurrentLink backupDir item
backupItem (CurrentLinkDoesntExist item backupDir) = do
  createCurrentLink backupDir item
  backupItem (ValidResult item)
backupItem (BackupDoesntExist item@(src, _)) = do
  backupDir <- backupDirectoryName item
  sh $ printf "rsync -a %s %s" src backupDir
  createCurrentLink backupDir item

backupItems = mapM_ backupItem

runBackup :: BackupConfigImpl () -> IO ()
runBackup config = do
  items  <- getBackupItems config
  result <- validateItems items
  backupItems result

backupMacos :: BackupConfigWriter config => config ()
backupMacos = do
  dest "/Volumes/MacExtraDrive/mac.bak"
  backup "safeplace"

main = runBackup backupMacos
