{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module TorrentFileSystem where
import Torrent
import TorrentTypes as TT
import Data.IORef (IORef)
import System.Posix.Types (COff)
import Data.List (partition)
import Data.Maybe (mapMaybe, isNothing, isJust, fromJust, fromMaybe)
import System.FilePath (splitFileName, splitDirectories, joinPath)
import Control.Lens
import System.IO (Handle)

import qualified Data.ByteString as B

import Debug.Trace

type TorrentFileSystemEntryList = [TorrentFileSystemEntry]
data TorrentFileSystemEntry = TFSTorrentFile { _torrent :: TorrentHandle
                                             , _name :: FilePath
                                             , _realPath :: FilePath
                                             , _filesize :: COff
                                             , _pieceStart :: TorrentPieceType
                                             , _pieceStartOffset :: TorrentPieceOffsetType
                                             , _pieceSize :: TorrentPieceSizeType }
                            -- | TFSTorrentDir { _torrent :: TorrentHandle, _name :: FilePath, _contents :: TorrentFileSystemEntryList }
                            | TFSFile { _name :: FilePath
                                      , _filesize :: COff }
                            | TFSDir { _name :: FilePath
                                     , _contents :: TorrentFileSystemEntryList }
                            deriving Show
makeLenses ''TorrentFileSystemEntry

data TFSHandle = SimpleFileHandle { _fileHandle :: Handle }
               | TorrentFileHandle { _fileNoBlock :: Bool
                                   , _tfsEntry :: TorrentFileSystemEntry
                                   , _blockCache :: IORef (Maybe (TorrentPieceType, B.ByteString))
                                   , _uid :: Word }
               | NewTorrentFileHandle (IORef B.ByteString)
makeLenses ''TFSHandle

mergeDirectories :: TorrentFileSystemEntryList -> TorrentFileSystemEntryList
mergeDirectories [] = []
mergeDirectories [x] = [x]
mergeDirectories (curr:xs) = let (same, notsame) = partition (\case
                                                                 -- cmp@TFSTorrentDir{} -> cmp^?torrent == curr^?torrent && cmp^.name == curr^.name
                                                                 cmp@TFSDir{} -> cmp^.name == curr^.name && isNothing (curr^?TorrentFileSystem.torrent)
                                                                 _ -> False) xs
                                 -- merge t1@TFSTorrentDir{} t2@TFSTorrentDir{} = t1 { _contents = mergeDirectories $ t1^.contents ++ t2^.contents }
                                 merge t1@TFSDir{} t2@TFSDir{} = t1 { _contents = mergeDirectories $ t1^.contents ++ t2^.contents }
                                 same' = foldr merge curr same
                               in same':mergeDirectories notsame



buildStructureFromTorrentInfo :: TorrentHandle -> TorrentInfo -> TorrentFileSystemEntryList
buildStructureFromTorrentInfo torrentHandle torrentInfo =
  let toTfsFile name torr = TFSTorrentFile { TorrentFileSystem._torrent = torrentHandle, _name = name, _realPath = joinPath [torrentInfo^.filesPath, torr^.TT.filename], TorrentFileSystem._filesize = torr^.TT.filesize, TorrentFileSystem._pieceStart = torr^.TT.pieceStart, TorrentFileSystem._pieceStartOffset = torr^.TT.pieceStartOffset, TorrentFileSystem._pieceSize = torrentInfo^.TT.pieceSize }
      splitname torrfile = let (dirs, filename) = splitFileName $ torrfile^.TT.filename
                               filteredDirs = case dirs of
                                                '.':'/':rest -> rest
                                                _ -> dirs
                             in (filteredDirs, filename)
      structure = flip map (torrentInfo^.torrentFiles) $ \torrfile -> let (dirs, file) = splitname torrfile in foldl (\child dir -> TFSDir dir [child]) (toTfsFile file torrfile) $ splitDirectories dirs
    in mergeDirectories $ traceShowId structure

getTFS :: TorrentFileSystemEntryList -> String -> TorrentFileSystemEntryList
getTFS files dirs = getTFS' files $ splitDirectories dirs
  where
    getTFS' [] _ = []
    getTFS' files [] = files
    getTFS' files (dir:xs) = unpackGetTFS' (filter (\a -> a^.name == dir) files) xs
    unpackGetTFS' files [] = files
    unpackGetTFS' files dir = getTFS' (concat $ mapMaybe (^?contents) files) dir

