{-# LANGUAGE ScopedTypeVariables #-}
import TorrentFileSystem
-- import TorrentTypes (TorrentHandle)
import qualified Data.ByteString.Char8 as B
import Data.Map.Strict hiding (splitAt, take)
import Data.Maybe (isJust)
import Control.Monad (forM)
import Foreign.ForeignPtr (newForeignPtr_, finalizeForeignPtr, touchForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.C.Types (CInt)
import System.Mem.Weak
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Map as Map
import qualified TorrentFileSystem as TFS
import Utils ((/^), (/%), (/.))
import Debug.Trace (traceShowM)

torrents :: [IO (FilePath, TorrentFileSystemEntry HotTorrent)]
torrents = flip fmap [("Torrent " ++ show i ++ ".torrent", stored $ TFSTorrentFile { _torrent = (), _hash = B.pack $ "I am torrent #" ++ show i, _filesize = 500, _pieceStart = 30, _pieceSize=90, _pieceStartOffset = 90, _singleFileTorrent = () }) | i <- [1..]] $ \(name, torrent) -> do
          ptr <- torrentPtr
          return (name, torrent { _torrent = ptr, _singleFileTorrent = False })

torrentPtr = newForeignPtr_ nullPtr

storedEntry :: IntoStored b => (a, b) -> (a, StoredType b)
storedEntry (name, e) = (name, intoStoredTorrent e)

storedEntries :: (IntoStored b, Traversable t) => t (a, b) -> t (a, StoredType b)
storedEntries = fmap storedEntry

hotEntries :: Traversable t => t (a, TorrentFileSystemEntry StoredTorrent) -> t (a, TorrentFileSystemEntry HotTorrent)
hotEntries = fmap $ \(a, e) -> (a, uninitialized e)

testMath offset count TFSTorrentFile {TFS._torrent = torrHandle, TFS._pieceStart = pieceStart, TFS._pieceSize = pieceSize, TFS._pieceStartOffset = pieceStartOffset, TFS._filesize = filesize}=
  let offset' = toInteger offset
      count' = max 0 (min (fromIntegral filesize - offset') $ toInteger count)
      pieceSize' = fromIntegral pieceSize
      pieceStartOffset' = fromIntegral pieceStartOffset
      firstPieceSize = pieceSize' - pieceStartOffset'
      (firstPiece, pieceOffset') = (offset' + pieceStartOffset') /% pieceSize'
      p@(piecesCount, _) = (count' + pieceOffset') /% pieceSize'
      piecesCount'
        | (q, 0) <- p,
          q > 0 = q - 1
        | otherwise = piecesCount
      currentReadLastPiece = firstPiece + piecesCount'
      _fileLastPiece = (fromIntegral filesize /. pieceSize') - (pieceStartOffset' /^ pieceSize')
      wantedPieces
        | count' == 0 = []
        | otherwise = [firstPiece..currentReadLastPiece]
  in traceShowM (("count", count'), ("firstPieceSize", firstPieceSize), ("pieceOffset", pieceOffset'), ("firstPiece", firstPiece), ("lastPiece", currentReadLastPiece), ("wanted", wantedPieces))

fsTests :: IO ()
fsTests = do
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
  let Just (torr4, merged2) = takeTFS merged "Some shit/More shit/Torrent 4.torrent"
      Just (torr5, merged3) = takeTFS merged2 "Some shit/More shit/Torrent 5.torrent"
      Just (torr1, merged4) = takeTFS merged3 "Some shit/Torrent 1.torrent"
      Just (torr9, merged5) = takeTFS merged4 "Torrent 9.torrent"
  putStrLn $ show [torr1, torr4, torr5, torr9]
  let newFiles = Map.singleton "Some shit" TFSDir {_contents = Map.fromList [("Blizzard 1.torrent", torr1), ("Was4.torr", torr4), ("Five.torrent", torr5), (".secret", TFSDir { _contents = Map.singleton "Secret.torrent" torr9})]}
      (merged6, New newFiles') = mergeDuplicatesFrom merged5 (New newFiles)
      merged7 = mergeDirectories2 merged6 (New newFiles')
  heading "Moved"
  putStrLn $ prettyFS merged7

  -- putStrLn $ show $ intoStoredTorrent dir1

otherThread a b = forkIO $ do
  --a' <- deRefWeak a
  --putStrLn $ if isJust a' then "A Exists"
  --                       else "A Does not exist"
  threadDelay 500000
  b' <- deRefWeak b
  putStrLn $ if isJust b' then "B Exists"
                         else "B Does not exist"
  a'' <- deRefWeak a
  putStrLn $ if isJust a'' then "A Exists"
                         else "A Does not exist"
  -- mapM touchForeignPtr a'
  return ()

foreignTest = do
  let p = nullPtr
      p2 = nullPtr
  alloca $ \(a :: Ptr CInt) -> do
    alloca $ \(b :: Ptr CInt) -> do
      a' <- newForeignPtr a (putStrLn "A Destroyed")
      b' <- newForeignPtr b (putStrLn "B Destroyed" >> touchForeignPtr a')
      wb <- mkWeakPtr b' Nothing
      wa <- mkWeakPtr a' Nothing
      otherThread wa wb
      threadDelay 200000
      finalizeForeignPtr b'
      finalizeForeignPtr a'
      threadDelay 1000000

main :: IO ()
main = fsTests >> foreignTest
