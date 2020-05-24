{-# LANGUAGE LambdaCase #-}
module TorrentFileSystem where
import Torrent
import Data.List (partition)
import Data.Maybe (mapMaybe, isNothing, isJust, fromJust, fromMaybe)
import System.FilePath (splitFileName, splitDirectories)

data TorrentFileSystemEntry = TFSTorrentFile TorrentHandle String
                            | TFSTorrentDir TorrentHandle String TorrentFileSystemEntryList
                            | TFSFile String Word
                            | TFSDir String TorrentFileSystemEntryList
                            deriving Show
type TorrentFileSystemEntryList = [TorrentFileSystemEntry]

tfsMetaData (TFSTorrentFile th name) = (Just th, name, Nothing)
tfsMetaData (TFSTorrentDir th name content) = (Just th, name, Just content)
tfsMetaData (TFSFile name _) = (Nothing, name, Nothing)
tfsMetaData (TFSDir name content) = (Nothing, name, Just content)

tfsContent a = let (_, _, content) = tfsMetaData a in content
tfsTorrent a = let (th, _, _) = tfsMetaData a in th
tfsName a = let (_, name, _) = tfsMetaData a in name

tfsFilesize (TFSFile _ size) = size
tfsFilesize _ = 0

mergeDirectories :: TorrentFileSystemEntryList -> TorrentFileSystemEntryList
mergeDirectories [] = []
mergeDirectories [x] = [x]
mergeDirectories (curr:xs) = let (th, name, content) = tfsMetaData curr
                                 (same, notsame) = partition (\case
                                                                 TFSTorrentDir th' name' content' -> Just th' == th && name' == name
                                                                 TFSDir name' content' -> name' == name && isNothing th
                                                                 _ -> False) xs
                                 merge (TFSTorrentDir th name content1) (TFSTorrentDir _ _ content2) = TFSTorrentDir th name (mergeDirectories $ content1 ++ content2)
                                 merge (TFSDir name content1) (TFSDir _ content2) = TFSDir name (mergeDirectories $ content1 ++ content2)
                                 same' = foldr merge curr same
                               in same':mergeDirectories notsame



buildStructureFromTorrents :: [TorrentFile] -> TorrentFileSystemEntryList
buildStructureFromTorrents filelist = let toTfsFile torr@(Torrent.TorrentFile _ size) = TFSFile (snd $ splitname torr) size
                                          splitname (Torrent.TorrentFile name _) = splitFileName name
                                          structure = flip map filelist $ \torrfile -> let (dirs, file) = splitname torrfile in foldl (\child dir -> TFSDir dir [child]) (toTfsFile torrfile) $ splitDirectories dirs
                                        in mergeDirectories structure

getTFS :: TorrentFileSystemEntryList -> String -> TorrentFileSystemEntryList
getTFS files dirs = getTFS' files $ splitDirectories dirs
  where 
    getTFS' [] _ = []
    getTFS' files [] = files
    getTFS' files (dir:xs) = unpackGetTFS' (filter (\a -> let (_, name, _) = tfsMetaData a in name == dir) files) xs
    unpackGetTFS' files [] = files
    unpackGetTFS' files dir = getTFS' (concat $ mapMaybe tfsContent files) dir

