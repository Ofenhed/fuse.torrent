{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
module TorrentFileSystem where
import Torrent
import TorrentTypes as TT
import Data.IORef (IORef)
import System.Posix.Types (COff)
import Data.Char (isDigit)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, isNothing, isJust, fromJust, fromMaybe)
import System.FilePath (splitFileName, splitDirectories, joinPath, equalFilePath)
import Control.Lens ((^.), (^?), (^?!), makeLenses)
import Control.Monad (void)
import System.IO (Handle)
import Text.ParserCombinators.ReadP

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Debug.Trace

type TorrentFileSystemEntryList = Map FilePath TorrentFileSystemEntry
data TorrentFileSystemEntry = TFSTorrentFile { _torrent :: TorrentHandle
                                             , _filesize :: COff
                                             , _pieceStart :: TorrentPieceType
                                             , _pieceStartOffset :: TorrentPieceOffsetType
                                             , _pieceSize :: TorrentPieceSizeType
                                             , _singleFileTorrent :: Bool }
                            | TFSTorrentDir { _torrent :: TorrentHandle, _contents :: TorrentFileSystemEntryList, _topLevelTorrentDir :: Bool }
                            | TFSFile { _filesize :: COff }
                            | TFSDir { _contents :: TorrentFileSystemEntryList }
                            deriving Show
makeLenses ''TorrentFileSystemEntry

type TorrentFd = Word

data TFSHandle = SimpleFileHandle { _fileHandle :: Handle }
               | TorrentFileHandle { _fileNoBlock :: Bool
                                   , _tfsEntry :: TorrentFileSystemEntry
                                   , _blockCache :: IORef (Maybe (TorrentPieceType, B.ByteString))
                                   , _uid :: TorrentFd }
               | NewTorrentFileHandle FilePath (IORef B.ByteString)
makeLenses ''TFSHandle

emptyFileSystem = Map.empty

data FilenameFormat = FilenameFormat { filenameBase :: FilePath
                                     , filenameCounter :: Maybe Word
                                     , filenameExtension :: Maybe FilePath }

instance Show FilenameFormat where
  show f = filenameBase f ++
           maybe [] (\c -> " (" ++ show c ++ ")") (filenameCounter f) ++
           maybe [] ("." ++) (filenameExtension f)

parseFilename :: Bool -> ReadP FilenameFormat
parseFilename hasExtension = do
  filename <- many get
  count <- option Nothing (Just . read <$> between (string " (") (char ')') (munch1 isDigit))
  ext <- if hasExtension
            then option Nothing (char '.' *> (Just <$> munch1 ('.' /=)))
            else return Nothing
  eof
  return $ FilenameFormat {filenameBase = filename, filenameCounter = count, filenameExtension = ext}

uncollide hasExtension filename siblings =
  let (parsed, "") = head $ readP_to_S (parseFilename hasExtension) filename
    in head $ flip mapMaybe [1..] $ \num -> let newName = show $ parsed { filenameCounter = Just $ num + fromMaybe 0 (filenameCounter parsed) }
                                              in if newName `elem` siblings
                                                    then Nothing
                                                    else Just newName

uncollide' TFSDir{} = uncollide False
uncollide' TFSTorrentDir{} = uncollide False
uncollide' _ = uncollide True

pathToTFSDir :: TorrentFileSystemEntryList -> FilePath -> TorrentFileSystemEntryList
pathToTFSDir content = foldl (\contents name -> Map.singleton name TFSDir{ _contents = contents}) content . reverse . filter (not . equalFilePath ".") . splitDirectories

pathToTFSDir' :: FilePath -> TorrentFileSystemEntryList
pathToTFSDir' = pathToTFSDir Map.empty

mergeDirectories :: [(FilePath, TorrentFileSystemEntry)] -> TorrentFileSystemEntryList
mergeDirectories = mergeDirectories' []
  where
  mergeDirectories' _ [] = Map.empty
  mergeDirectories' _ [(name, file)] = Map.singleton name file
  mergeDirectories' siblings ((currName, curr):xs) =
    let (same, notsame) = flip partition xs $ (==)currName . fst
        (compatile, uncompatible) = flip partition same $ \other ->
          case (other, curr) of
            ((_, TFSDir{}), cmp@TFSDir{}) -> True
            ((_, TFSDir{}), cmp@TFSTorrentDir{}) -> True
            ((_, TFSTorrentDir{}), cmp@TFSDir{}) -> True
            ((_, TFSTorrentDir{ _torrent = torrent1 }), TFSTorrentDir{ _torrent = torrent2 }) -> torrent1 == torrent2
            _ -> False
        merge (_, t1@TFSDir{}) t2@TFSDir{} = t1 { _contents = mergeDirectories $ Map.toList (t1^.contents) ++ Map.toList (t2^.contents) }
        merge (_, t1@TFSTorrentDir{}) t2@TFSDir{} = t1 { _contents = mergeDirectories $ Map.toList (t1^.contents) ++ Map.toList (t2^.contents) }
        merge (_, t1@TFSDir{}) t2@TFSTorrentDir{} = t2 { _contents = mergeDirectories $ Map.toList (t1^.contents) ++ Map.toList (t2^.contents) }
        merge (_, t1@TFSTorrentDir{}) t2@TFSTorrentDir{} = t1 { _contents = mergeDirectories $ Map.toList (t1^.contents) ++ Map.toList (t2^.contents) }
        (newSiblings, renamed) = foldl (\(siblings', result) (uncompName, uncompData) ->
                    let newname = uncollide' uncompData uncompName siblings'
                      in (newname:siblings', (newname, uncompData):result)) (siblings, []) uncompatible
        same' = foldr merge curr compatile
      in Map.insert currName same' $ Map.union (Map.fromList renamed) $ mergeDirectories' (newSiblings ++ currName:siblings) notsame

mergeDirectories2 :: TorrentFileSystemEntryList -> TorrentFileSystemEntryList -> TorrentFileSystemEntryList
mergeDirectories2 d1 d2 = mergeDirectories $ Map.toList d1 ++ Map.toList d2

buildStructureFromTorrentInfo :: TorrentHandle -> TorrentInfo -> TorrentFileSystemEntryList
buildStructureFromTorrentInfo torrentHandle torrentInfo =
  let toTfsFile torr = TFSTorrentFile { TorrentFileSystem._torrent = torrentHandle, TorrentFileSystem._filesize = torr^.TT.filesize, TorrentFileSystem._pieceStart = torr^.TT.pieceStart, TorrentFileSystem._pieceStartOffset = torr^.TT.pieceStartOffset, TorrentFileSystem._pieceSize = torrentInfo^.TT.pieceSize, _singleFileTorrent = length (torrentInfo^.torrentFiles) == 1 }
      splitname torrfile = let (dirs, filename) = splitFileName $ torrfile^.TT.filename
                               filteredDirs = case dirs of
                                                '.':'/':rest -> rest
                                                '/':rest -> rest
                                                _ -> dirs
                             in (filteredDirs, filename)
      structure = flip map (torrentInfo^.torrentFiles) $ \torrfile -> let (dirs, file) = splitname torrfile in foldl (\(childname, child) dir -> (dir, TFSTorrentDir { _torrent = torrentHandle, _topLevelTorrentDir = False, _contents = Map.singleton childname child})) (file, toTfsFile torrfile) $ splitDirectories dirs
      merged = mergeDirectories structure
      topDirToTorrent = if Map.size merged /= 1
                           then merged
                           else flip Map.map merged $
                             \case
                                TFSDir { _contents = contents } -> TFSTorrentDir { _torrent = torrentHandle, _contents = contents, _topLevelTorrentDir = True }
                                x -> x
    in traceShowId topDirToTorrent

getTFS' :: TorrentFileSystemEntryList -> FilePath -> Maybe (TorrentFileSystemEntry, [FilePath])
getTFS' files = getTFS'' [] (Just files) . splitDirectories
  where
    getTFS'' ((name, last):rest) _ [] = Just (last, name:map fst rest)
    getTFS'' _ Nothing (_:_) = Nothing
    getTFS'' walk (Just files) (dir:xs) = case Map.lookup dir files of
                                            Just m -> getTFS'' ((dir, m):walk) (m^?contents) xs
                                            Nothing -> Nothing


getTFS :: TorrentFileSystemEntryList -> String -> [(FilePath, TorrentFileSystemEntry)]
getTFS files dir = case getTFS' files dir of
                     Just (dir@TFSDir{}, name:_) -> Map.toList $ dir^?!contents
                     Just (dir@TFSTorrentDir{}, name:_) -> Map.toList $ dir^?!contents
                     Just (file@TFSFile{}, name:_) -> [(name, file)]
                     Just (file@TFSTorrentFile{}, name:_) -> [(name, file)]
                     Nothing -> []
