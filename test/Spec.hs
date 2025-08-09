import TorrentFileSystem
import TorrentTypes (TorrentHandle)
import qualified Data.ByteString.Char8 as B
import Data.Map.Strict hiding (splitAt, take)
import Control.Monad (forM)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (nullPtr)
import qualified Data.Map as Map

torrents :: [IO (FilePath, TorrentFileSystemEntry HotTorrent)]
torrents = flip fmap [("Torrent " ++ show i ++ ".torrent", stored $ TFSTorrentFile { _torrent = (), _hash = B.pack $ "I am torrent #" ++ show i, _filesize = 500, _pieceStart = 30, _pieceSize=90, _pieceStartOffset = 90, _singleFileTorrent = () }) | i <- [1..]] $ \(name, torrent) -> do
          ptr <- torrentPtr
          return (name, torrent { _torrent = ptr, _singleFileTorrent = False })

torrentPtr :: IO TorrentHandle
torrentPtr = newForeignPtr_ nullPtr

storedEntry :: IntoStored b => (a, b) -> (a, StoredType b)
storedEntry (name, e) = (name, intoStoredTorrent e)

storedEntries :: (IntoStored b, Traversable t) => t (a, b) -> t (a, StoredType b)
storedEntries = fmap storedEntry

hotEntries :: Traversable t => t (a, TorrentFileSystemEntry StoredTorrent) -> t (a, TorrentFileSystemEntry HotTorrent)
hotEntries = fmap $ \(a, e) -> (a, uninitialized e)

main :: IO ()
main = do
  let torrents' = torrents
      (t1, t2') = splitAt 3 torrents'
      (t2, t3') = splitAt 3 t2'
      t3 = take 5 t3'
      heading = putStrLn . ('\n':)
  dir1 <- mapM id t1
  dir2 <- mapM id t2
  dir3 <- mapM id t3
  resolvedFiles <- mapM (\x -> x >>= \(name, e) -> return ("New " ++ name, e)) [torrents' !! 2, torrents' !! 4, torrents' !! 0, torrents' !! 15]
  let dirs = Map.fromList $ hotEntries $ ("Some shit", TFSDir { _contents = Map.fromList $ ("More shit", TFSDir {_contents = Map.fromList $ storedEntries dir2}):storedEntries dir1 }):storedEntries dir3
  let newTorrent = Map.fromList $ [("My new torrent", TFSDir { _contents = Map.fromList resolvedFiles })]
  putStrLn $ prettyFS $ dirs
  putStrLn $ prettyFS $ newTorrent
  heading "Merging duplicates"
  let (dirs', New newTorrent') = mergeDuplicatesFrom dirs (New newTorrent)
  putStrLn $ prettyFS $ dirs'
  heading "Remaining"
  putStrLn $ prettyFS $ newTorrent'
  heading "Merging structure"
  let merged = mergeDirectories2 dirs' (New newTorrent')
  putStrLn $ prettyFS merged
  heading "Taking files"
  let Just (torr4, merged2) = takeTFS "Some shit/More shit/Torrent 4.torrent" merged
      Just (torr5, merged3) = takeTFS "Some shit/More shit/Torrent 5.torrent" merged2
      Just (torr1, merged4) = takeTFS "Some shit/Torrent 1.torrent" merged3
      Just (torr9, merged5) = takeTFS "Torrent 9.torrent" merged4
  putStrLn $ show [torr1, torr4, torr5, torr9]
  let newFiles = Map.singleton "Some shit" TFSDir {_contents = Map.fromList [("Blizzard 1.torrent", torr1), ("Was4.torr", torr4), ("Five.torrent", torr5), (".secret", TFSDir { _contents = Map.singleton "Secret.torrent" torr9})]}
      (merged6, New newFiles') = mergeDuplicatesFrom merged5 (New newFiles)
      merged7 = mergeDirectories2 merged6 (New newFiles')
  heading "Moved"
  putStrLn $ prettyFS merged7

  -- putStrLn $ show $ intoStoredTorrent dir1
