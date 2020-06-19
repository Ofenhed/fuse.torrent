{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
module TorrentFileSystem where
import Torrent
import TorrentTypes as TT
import Data.IORef (IORef)
import System.Posix.Types (COff)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, isNothing, isJust, fromJust, fromMaybe)
import System.FilePath (splitFileName, splitDirectories, joinPath)
import Control.Lens
import System.IO (Handle)

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
                            | TFSTorrentDir { _torrent :: TorrentHandle, _contents :: TorrentFileSystemEntryList }
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
               | NewTorrentFileHandle (IORef B.ByteString)
makeLenses ''TFSHandle

emptyFileSystem = Map.empty

mergeDirectories :: [(FilePath, TorrentFileSystemEntry)] -> TorrentFileSystemEntryList
mergeDirectories [] = Map.empty
mergeDirectories [(name, file)] = Map.singleton name file
mergeDirectories ((currName, curr):xs) =
  let (same, notsame) = flip partition xs $
         \(key, value) -> case (key, value) of
           -- cmp@TFSTorrentDir{} -> cmp^?torrent == curr^?torrent && cmp^.name == curr^.name
           (name, cmp@TFSDir{}) -> name == currName && isNothing (curr^?TorrentFileSystem.torrent)
           _ -> False
      -- merge t1@TFSTorrentDir{} t2@TFSTorrentDir{} = t1 { _contents = mergeDirectories $ t1^.contents ++ t2^.contents }
      merge (_, t1@TFSDir{}) t2@TFSDir{} = t1 { _contents = mergeDirectories $ Map.toList (t1^.contents) ++ Map.toList (t2^.contents) }
      same' = foldr merge curr same
    in Map.insert currName same' $ mergeDirectories notsame

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
      structure = flip map (torrentInfo^.torrentFiles) $ \torrfile -> let (dirs, file) = splitname torrfile in foldl (\(childname, child) dir -> (dir, TFSDir $ Map.singleton childname child)) (file, toTfsFile torrfile) $ splitDirectories dirs
      merged = mergeDirectories structure
      topDirToTorrent = if Map.size merged /= 1
                           then merged
                           else flip Map.map merged $
                             \case
                                TFSDir { _contents = contents } -> TFSTorrentDir { _torrent = torrentHandle, _contents = contents }
                                x -> x
    in traceShowId $ topDirToTorrent

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
