{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module TorrentFileSystem where
import Torrent
import TorrentTypes as TT
import Data.List (partition)
import Data.Maybe (mapMaybe, isNothing, isJust, fromJust, fromMaybe)
import System.FilePath (splitFileName, splitDirectories)
import Control.Lens

type TorrentFileSystemEntryList = [TorrentFileSystemEntry]
data TorrentFileSystemEntry = TFSTorrentFile { _torrent :: TorrentHandle, _filesize :: Word, _name :: FilePath, _partStart :: TorrentPieceType, _partStartOffset :: TorrentPieceOffsetType, _partSize :: TorrentPieceSizeType }
                            | TFSTorrentDir { _torrent :: TorrentHandle, _name :: FilePath, _contents :: TorrentFileSystemEntryList }
                            | TFSFile { _name :: FilePath, _filesize :: Word }
                            | TFSDir { _name :: FilePath, _contents :: TorrentFileSystemEntryList }
                            deriving Show
makeLenses ''TorrentFileSystemEntry

mergeDirectories :: TorrentFileSystemEntryList -> TorrentFileSystemEntryList
mergeDirectories [] = []
mergeDirectories [x] = [x]
mergeDirectories (curr:xs) = let (same, notsame) = partition (\case
                                                                 TFSTorrentDir th' name' content' -> Just th' == curr^?torrent && name' == curr^.name
                                                                 TFSDir name' content' -> name' == curr^.name && isNothing (curr^?torrent)
                                                                 _ -> False) xs
                                 merge t1@TFSTorrentDir{} t2@TFSTorrentDir{} = t1 { _contents = mergeDirectories $ t1^.contents ++ t2^.contents }
                                 merge t1@TFSDir{} t2@TFSDir{} = t1 { _contents = mergeDirectories $ t1^.contents ++ t2^.contents }
                                 same' = foldr merge curr same
                               in same':mergeDirectories notsame



buildStructureFromTorrents :: [TorrentFile] -> TorrentFileSystemEntryList
buildStructureFromTorrents filelist = let toTfsFile torr = TFSFile (snd $ splitname torr) $ view TT.filesize torr
                                          splitname = splitFileName . view TT.filename
                                          structure = flip map filelist $ \torrfile -> let (dirs, file) = splitname torrfile in foldl (\child dir -> TFSDir dir [child]) (toTfsFile torrfile) $ splitDirectories dirs
                                        in mergeDirectories structure

getTFS :: TorrentFileSystemEntryList -> String -> TorrentFileSystemEntryList
getTFS files dirs = getTFS' files $ splitDirectories dirs
  where 
    getTFS' [] _ = []
    getTFS' files [] = files
    getTFS' files (dir:xs) = unpackGetTFS' (filter (\a -> a^.name == dir) files) xs
    unpackGetTFS' files [] = files
    unpackGetTFS' files dir = getTFS' (concat $ mapMaybe (^?contents) files) dir

