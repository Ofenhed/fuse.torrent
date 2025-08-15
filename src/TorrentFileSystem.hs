{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module TorrentFileSystem where

import Control.Concurrent.STM (TVar)
import Control.Lens (makeLenses, (^.), (^?), (^?!))
import Control.Monad (join)
import Control.Monad.State (MonadState, runState)
import qualified Control.Monad.State.Lazy as MS
import qualified Data.ByteString as B
import Data.Char (isDigit)
import Data.List (intercalate, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Typeable (Typeable, cast, gcast)
import Debug.Trace
import System.FilePath (equalFilePath, splitDirectories, splitFileName)
import System.IO (Handle)
import System.Posix.Types (COff, FileOffset)
import Text.ParserCombinators.ReadP (between, char, eof, get, many, munch1, option, readP_to_S, string)
import Torrent
import TorrentTypes as TT

type TorrentFileSystemEntryList' st ba = Map FilePath (TorrentFileSystemEntry st ba)

type TorrentFileSystemEntryList st = TorrentFileSystemEntryList' st HotTorrent

-- data StoredTorrentHandle where
--   KnownTorrent :: { _handle :: TorrentHandle,
--                     _hash :: TorrentHash } -> StoredTorrentHandle
--   StoredHash :: { _hash :: TorrentHash } -> StoredTorrentHandle
--
-- instance Eq StoredTorrentHandle where
--   a == b = _hash a == _hash b
--
-- instance Show StoredTorrentHandle where
--   showsPrec d = showsPrec d . _hash
--
-- instance Read StoredTorrentHandle where
--   readsPrec d = fmap (\(x, r) -> (StoredHash { _hash = x}, r)) . readsPrec d
--
-- torrentHandle KnownTorrent { _handle = h } = Just h
-- torrentHandle StoredHash {} = Nothing

-- TODO: Serialize and deserialize a subset of this
-- data TorrentFileSystemEntry st where
--   TFSTorrentFile ::
--       { _torrent :: StoredTorrentHandle,
--         _filesize :: COff,
--         _pieceStart :: TorrentPieceType,
--         _pieceStartOffset :: TorrentPieceOffsetType,
--         _pieceSize :: TorrentPieceSizeType,
--         _singleFileTorrent :: Bool
--       } -> TorrentFileSystemEntry st
--   TFSTorrentDir :: {_torrent :: StoredTorrentHandle, _contents :: TorrentFileSystemEntryList st, _topLevelTorrentDir :: Bool} -> TorrentFileSystemEntry st
--   TFSFile :: {_filesize :: COff} -> TorrentFileSystemEntry st
--   TFSDir :: {_contents :: TorrentFileSystemEntryList st} -> TorrentFileSystemEntry st

type family TorrentStorageBackend a b where
  TorrentStorageBackend StoredTorrent a = ()
  TorrentStorageBackend HotTorrent a = a

data HotTorrent

data StoredTorrent

class TorrentStorageBackends a

instance TorrentStorageBackends HotTorrent

instance TorrentStorageBackends StoredTorrent

data TorrentFileSystemEntry st ba where
  TFSTorrentFile ::
    { _torrent :: TorrentStorageBackend ba TorrentHandle,
      _hash :: TorrentHash,
      _filesize :: COff,
      _pieceStart :: TorrentPieceType,
      _pieceStartOffset :: TorrentPieceOffsetType,
      _pieceSize :: TorrentPieceSizeType,
      _singleFileTorrent :: TorrentStorageBackend ba Bool,
      _attributes :: st
    } ->
    TorrentFileSystemEntry st ba
  TFSFile :: {_filesize :: COff, _attributes :: st} -> TorrentFileSystemEntry st ba
  TFSUninitialized :: TorrentFileSystemEntry st StoredTorrent -> TorrentFileSystemEntry st ba
  TFSDir :: {_contents :: TorrentFileSystemEntryList' st ba, _attributes :: st} -> TorrentFileSystemEntry st ba

data PrettyPrintFS where
  PrettyFS :: forall st ba. (Show st, Show (TorrentFileSystemEntry st ba)) => TorrentFileSystemEntryList' st ba -> PrettyPrintFS
  PrettyLine :: forall st ba. (Show st, Show (TorrentFileSystemEntry st ba)) => (FilePath, TorrentFileSystemEntry st ba) -> PrettyPrintFS
  PrettyIndentFS :: PrettyPrintFS -> PrettyPrintFS

split :: (Eq a) => a -> [a] -> [[a]]
split d = split' []
  where
    split' s x
      | (x' : xs) <- x,
        x' == d =
          reverse s : split' [] xs
      | (x' : xs) <- x = split' (x' : s) xs
      | otherwise = [reverse s]

instance Show PrettyPrintFS where
  show (PrettyFS files) = intercalate "\n" $ fmap (show . PrettyLine) $ Map.toList files
  show (PrettyLine (name, TFSTorrentFile {})) = name
  show (PrettyLine (name, TFSFile {})) = name ++ " ☠"
  show (PrettyLine (name, TFSDir {_contents = c})) = name ++ "/\n" ++ show (PrettyIndentFS (PrettyFS c))
  show (PrettyLine (name, TFSUninitialized i)) = intercalate "\n" $ fmap ("❄" ++) $ split '\n' (show $ PrettyLine (name, i))
  show (PrettyIndentFS x) = intercalate "\n" $ fmap ("  " ++) $ split '\n' (show x)

prettyFS :: (Show st, Show (TorrentFileSystemEntry st ba)) => TorrentFileSystemEntryList' st ba -> String
prettyFS x = show $ PrettyFS x

stored :: TorrentFileSystemEntry st StoredTorrent -> TorrentFileSystemEntry st StoredTorrent
stored = id

uninitialized :: TorrentFileSystemEntry st StoredTorrent -> TorrentFileSystemEntry st HotTorrent
uninitialized a@TFSTorrentFile {} = TFSUninitialized a
-- uninitialized a@TFSTorrentDir {} = TFSUninitialized a
uninitialized (TFSUninitialized a) = TFSUninitialized a
uninitialized TFSFile {TorrentFileSystem._filesize = f, _attributes = a} = TFSFile {TorrentFileSystem._filesize = f, TorrentFileSystem._attributes = a}
uninitialized d@TFSDir {_contents = c} = d {_contents = fmap uninitialized c}

deriving instance (Show st) => Show (TorrentFileSystemEntry st StoredTorrent)

deriving instance (Read st) => Read (TorrentFileSystemEntry st StoredTorrent)

deriving instance (Eq st) => Eq (TorrentFileSystemEntry st StoredTorrent)

deriving instance (Show st) => Show (TorrentFileSystemEntry st HotTorrent)

-- instance Show (TorrentFileSystemEntry st HotTorrent) where
--   showsPrec d (TFSUninitialized i) = showsPrec d i
--   showsPrec d i@TFSTorrentFile {} = showParen (d > 10) $ showString "Hot " . showsPrec (d+1) (intoStoredTorrent i)
--   showsPrec d i@TFSTorrentDir {_contents = c, _hash = h} = showParen (d > 10) $ showString ("Hot (" ++ C8.unpack h ++ ")") . showsPrec (d+1) (fmap intoStoredTorrent c)
--   showsPrec d i@TFSDir {_contents = c} = showParen (d > 10) $ showString "Hot " . showsPrec (d+1) (fmap intoStoredTorrent c)

class IntoStored a where
  type StoredType a
  intoStoredTorrent :: a -> StoredType a

instance IntoStored (TorrentFileSystemEntry st HotTorrent) where
  type StoredType (TorrentFileSystemEntry st HotTorrent) = TorrentFileSystemEntry st StoredTorrent
  intoStoredTorrent t@TFSTorrentFile {} = t {_torrent = (), _singleFileTorrent = ()}
  intoStoredTorrent d@TFSDir {_contents = c} = d {_contents = fmap intoStoredTorrent c}
  -- intoStoredTorrent t@TFSTorrentDir { _contents = c} = t { _torrent = (), _contents = fmap intoStoredTorrent c }
  intoStoredTorrent (TFSUninitialized t) = t

instance IntoStored (TorrentFileSystemEntry st StoredTorrent) where
  type StoredType (TorrentFileSystemEntry st StoredTorrent) = TorrentFileSystemEntry st StoredTorrent
  intoStoredTorrent = id

instance (IntoStored v) => IntoStored (Map a v) where
  type StoredType (Map a v) = Map a (StoredType v)
  intoStoredTorrent = fmap intoStoredTorrent

class DefaultAttributes a where
  defaultFileAttrs :: a
  defaultDirAttrs :: a

makeLenses ''TorrentFileSystemEntry

type TorrentFd = Word

data TFSHandle st
  = SimpleFileHandle {_fileHandle :: Handle}
  | TorrentFileHandle
      { _fileNoBlock :: Bool,
        _tfsEntry :: TorrentFileSystemEntry st HotTorrent,
        _blockCache :: TVar [(TorrentPieceType, B.ByteString)],
        _uid :: TorrentFd,
        _lastRequest :: TVar (Maybe FileOffset)
      }
  | NewTorrentFileHandle FilePath (TVar B.ByteString)

makeLenses ''TFSHandle

emptyFileSystem = Map.empty

data FilenameFormat = FilenameFormat
  { filenameBase :: FilePath,
    filenameCounter :: Maybe Word,
    filenameExtension :: Maybe FilePath
  }

instance Show FilenameFormat where
  showsPrec d f =
    showString
      ( filenameBase f
          ++ maybe [] (\c -> " (" ++ show c ++ ")") (filenameCounter f)
          ++ maybe [] (\ext -> '.' : ext) (filenameExtension f)
      )

instance Read FilenameFormat where
  readsPrec _ = readP_to_S $ do
    filename <- many get
    count <- option Nothing (Just . read <$> between (string " (") (char ')') (munch1 isDigit))
    ext <- option Nothing (char '.' *> (Just <$> munch1 ('.' /=)))
    eof
    return $ FilenameFormat {filenameBase = filename, filenameCounter = count, filenameExtension = ext}

uncollide hasExtension filename siblings = traceShow
  ("Parsing filename", filename)
  listToMaybe
  $ join
  $ flip fmap (read filename)
  $ \(parsed, "") ->
    flip mapMaybe [1 ..] $ \num ->
      let newName = show $ parsed {filenameCounter = Just $ num + fromMaybe 0 (filenameCounter parsed)}
       in if newName `elem` siblings
            then Nothing
            else Just newName

uncollide' TFSDir {} = uncollide False
-- uncollide' TFSTorrentDir {} = uncollide False
uncollide' _ = uncollide True

pathToTFSDir :: TorrentFileSystemEntryList st -> FilePath -> TorrentFileSystemEntryList st
pathToTFSDir content = foldl (\contents name -> Map.singleton name TFSDir {_contents = contents}) content . reverse . filter (not . equalFilePath ".") . splitDirectories

pathToTFSDir' :: FilePath -> TorrentFileSystemEntryList st
pathToTFSDir' = pathToTFSDir Map.empty

mergeDirectories :: [(FilePath, TorrentFileSystemEntry st HotTorrent)] -> TorrentFileSystemEntryList st
mergeDirectories = mergeDirectories' []
  where
    mergeDirectories' _ [] = Map.empty
    mergeDirectories' _ [(name, file)] = Map.singleton name file
    mergeDirectories' siblings ((currName, curr) : xs) =
      let (same, notsame) = flip partition xs $ (==) currName . fst
          (compatile, uncompatible) = flip partition same $ \other ->
            case (other, curr) of
              ((_, TFSDir {}), cmp@TFSDir {}) -> True
              -- ((_, TFSDir {}), cmp@TFSTorrentDir {}) -> True
              -- ((_, TFSTorrentDir {}), cmp@TFSDir {}) -> True
              -- ((_, TFSTorrentDir {_torrent = torrent1}), TFSTorrentDir {_torrent = torrent2}) -> torrent1 == torrent2
              _ -> False
          merge (_, t1@TFSDir {}) t2@TFSDir {} = t1 {_contents = mergeDirectories $ Map.toList (t1 ^. contents) ++ Map.toList (t2 ^. contents)}
          -- merge (_, t1@TFSTorrentDir {}) t2@TFSDir {} = t1 {_contents = mergeDirectories $ Map.toList (t1 ^. contents) ++ Map.toList (t2 ^. contents)}
          -- merge (_, t1@TFSDir {}) t2@TFSTorrentDir {} = t2 {_contents = mergeDirectories $ Map.toList (t1 ^. contents) ++ Map.toList (t2 ^. contents)}
          -- merge (_, t1@TFSTorrentDir {}) t2@TFSTorrentDir {} = t1 {_contents = mergeDirectories $ Map.toList (t1 ^. contents) ++ Map.toList (t2 ^. contents)}
          (newSiblings, renamed) =
            foldl
              ( \(siblings', result) (uncompName, uncompData) ->
                  let Just newname = uncollide' uncompData uncompName siblings'
                   in (newname : siblings', (newname, uncompData) : result)
              )
              (siblings, [])
              uncompatible
          same' = foldr merge curr compatile
       in Map.insert currName same' $ Map.union (Map.fromList renamed) $ mergeDirectories' (newSiblings ++ currName : siblings) notsame

filterFS :: (TorrentFileSystemEntry st HotTorrent -> Bool) -> [(FilePath, TorrentFileSystemEntry st HotTorrent)] -> Maybe (TorrentFileSystemEntry st HotTorrent, [(FilePath, TorrentFileSystemEntry st HotTorrent)])
filterFS f ((name, x) : xs)
  | x'@TFSDir {_contents = c} <- x,
    Just (found, newMap) <- filterFS f (Map.toList c) =
      let newSelf = (name, x' {_contents = Map.fromList newMap}) : xs
          newSelf' = filterFS f newSelf
       in Just $ fromMaybe (found, newSelf) newSelf'
  | f x = Just (x, xs)
  | Just (matched, t) <- filterFS f xs = Just (matched, (name, x) : t)
  | otherwise = Nothing
filterFS _ [] = Nothing

data New a = New a

mergeDuplicatesFrom :: (Eq (TorrentFileSystemEntry st StoredTorrent)) => TorrentFileSystemEntryList' st HotTorrent -> New (TorrentFileSystemEntryList' st HotTorrent) -> (TorrentFileSystemEntryList' st HotTorrent, New (TorrentFileSystemEntryList' st HotTorrent))
mergeDuplicatesFrom old (New new) =
  let (old', new') = runState mergeDuplicatesFrom' new
   in (Map.fromList old', New new')
  where
    mergeDuplicatesFrom' = do
      r <- withAll $ Map.toList old
      removeEmpty
      return r
    removeEmptyFilter :: forall st bt. TorrentFileSystemEntry st bt -> Bool
    removeEmptyFilter (TFSUninitialized a) = removeEmptyFilter a
    removeEmptyFilter TFSDir {_contents = c}
      | null c = True
      | otherwise = False
    removeEmptyFilter _ = False
    removeEmpty :: (MonadState (TorrentFileSystemEntryList st) m) => m ()
    removeEmpty = MS.modify $ \files -> fromMaybe files $ fmap (Map.fromList . snd) $ filterFS removeEmptyFilter $ Map.toList files
    withAll :: (Eq (TorrentFileSystemEntry st StoredTorrent), MonadState (TorrentFileSystemEntryList' st HotTorrent) m) => [(FilePath, TorrentFileSystemEntry st HotTorrent)] -> m [(FilePath, TorrentFileSystemEntry st HotTorrent)]
    withAll [] = return []
    withAll ((name, TFSUninitialized x) : xs) = do
      newData <- withUninitialized x
      newSelf <- case newData of
        Left a -> return $ TFSUninitialized a
        Right a -> return a
      rest <- withAll xs
      return $ (name, newSelf) : rest
    withAll ((name, (d@TFSDir {_contents = c})) : xs) = do
      newC <- withAll $ Map.toList c
      rest <- withAll xs
      return ((name, (d {_contents = Map.fromList newC})) : rest)
    withAll ((name, a) : xs) = do
      rest <- withAll xs
      return $ (name, a) : rest
    withUninitialized :: (Eq (TorrentFileSystemEntry st StoredTorrent), MonadState (TorrentFileSystemEntryList' st HotTorrent) m) => TorrentFileSystemEntry st StoredTorrent -> m (Either (TorrentFileSystemEntry st StoredTorrent) (TorrentFileSystemEntry st HotTorrent))
    withUninitialized old@TFSDir {_contents = c} = do
      applied <- mapM (\(name, e) -> withUninitialized e >>= return . (name,)) $ Map.toList c
      let intoResponse :: (FilePath, Either (TorrentFileSystemEntry st StoredTorrent) (TorrentFileSystemEntry st HotTorrent)) -> Either [(FilePath, TorrentFileSystemEntry st StoredTorrent)] [(FilePath, TorrentFileSystemEntry st HotTorrent)] -> Either [(FilePath, TorrentFileSystemEntry st StoredTorrent)] [(FilePath, TorrentFileSystemEntry st HotTorrent)]
          intoResponse (name, Left next) (Left files) = Left ((name, next) : files)
          intoResponse (name, Right next) (Left files) = Right ((name, next) : fmap (\(name, file) -> (name, TFSUninitialized file)) files)
          intoResponse (name, Right next) (Right files) = Right ((name, next) : files)
          intoResponse (name, Left next) (Right files) = Right ((name, (TFSUninitialized next)) : files)
          folded = foldr intoResponse (Left []) applied
      return $ case folded of
        Left notInitialized -> Left old {_contents = Map.fromList notInitialized}
        Right initialized -> Right old {_contents = Map.fromList initialized}
    withUninitialized old@TFSTorrentFile {} = do
      new <- MS.get
      case filterFS ((==) old . intoStoredTorrent) (Map.toList new) of
        Just (newData, new') -> do
          MS.put $ Map.fromList new'
          return $ Right newData
        Nothing -> return $ Left old

mergeDirectories2 :: TorrentFileSystemEntryList st -> New (TorrentFileSystemEntryList st) -> TorrentFileSystemEntryList st
mergeDirectories2 d1 (New d2) = mergeDirectories $ Map.toList d1 ++ Map.toList d2

mergeDirectories2' :: (Eq (TorrentFileSystemEntry st StoredTorrent)) => TorrentFileSystemEntryList st -> New (TorrentFileSystemEntryList st) -> TorrentFileSystemEntryList st
mergeDirectories2' d1 d2 =
  let (d1', New d2') = mergeDuplicatesFrom d1 d2
   in mergeDirectories2 d1' (New d2')

toTFSDir :: (Typeable ba, DefaultAttributes st) => FilePath -> TorrentFileSystemEntry st ba -> Maybe (TorrentFileSystemEntryList st)
toTFSDir "/" = const Nothing
toTFSDir p = toTFSDir' (splitDirectories p)
  where
    toTFSDir' ("/" : xs) = toTFSDir' xs
    toTFSDir' ("." : xs) = toTFSDir' xs
    toTFSDir' (".." : xs) = toTFSDir' xs
    toTFSDir' [name] = Just . Map.singleton name . hotBackend
    toTFSDir' (name : ns) = fmap (\e' -> Map.singleton name $ TFSDir {_contents = e', _attributes = defaultDirAttrs}) . toTFSDir' ns
    toTFSDir' [] = const Nothing

buildStructureFromTorrentInfo :: (Show st, DefaultAttributes st) => (TorrentHandle, TorrentHash) -> TorrentInfo -> TorrentFileSystemEntryList st
buildStructureFromTorrentInfo (torrentHandle, hash) torrentInfo =
  let toTfsFile torr = TFSTorrentFile {TorrentFileSystem._torrent = torrentHandle, TorrentFileSystem._hash = hash, TorrentFileSystem._filesize = torr ^. TT.filesize, TorrentFileSystem._pieceStart = torr ^. TT.pieceStart, TorrentFileSystem._pieceStartOffset = torr ^. TT.pieceStartOffset, TorrentFileSystem._pieceSize = torrentInfo ^. TT.pieceSize, _singleFileTorrent = length (torrentInfo ^. torrentFiles) == 1, _attributes = defaultFileAttrs}
      splitname torrfile =
        let (dirs, filename) = splitFileName $ torrfile ^. TT.filename
            filteredDirs = case dirs of
              '.' : '/' : rest -> rest
              '/' : rest -> rest
              _ -> dirs
         in (filteredDirs, filename)
      structure = flip map (torrentInfo ^. torrentFiles) $ \torrfile -> let (dirs, file) = splitname torrfile in foldl (\(childname, child) dir -> (dir, TFSDir {_contents = Map.singleton childname child, _attributes = defaultDirAttrs})) (file, toTfsFile torrfile) $ splitDirectories dirs
      merged = mergeDirectories structure
      topDirToTorrent =
        if Map.size merged /= 1
          then merged
          else flip Map.map merged $
            \case
              x@TFSDir {_contents = c} -> x {_contents = c}
              x -> x
   in traceShowId topDirToTorrent

eitherBackend :: (Typeable a) => TorrentFileSystemEntry st a -> Either (TorrentFileSystemEntry st StoredTorrent) (TorrentFileSystemEntry st HotTorrent)
eitherBackend e
  | TFSUninitialized a <- e = eitherBackend a
  | Just stored' <- gcast e = Left stored'
  | Just hot <- gcast e = Right hot
  | otherwise = error "Invalid file system entry type"

hotBackend' :: Either (TorrentFileSystemEntry st StoredTorrent) (TorrentFileSystemEntry st HotTorrent) -> TorrentFileSystemEntry st HotTorrent
hotBackend' (Left l) = TFSUninitialized l
hotBackend' (Right r) = r

hotBackend :: (Typeable a) => TorrentFileSystemEntry st a -> TorrentFileSystemEntry st HotTorrent
hotBackend = hotBackend' . eitherBackend

assertHotBackend :: (Typeable a) => TorrentFileSystemEntry st a -> Maybe (TorrentFileSystemEntry st HotTorrent)
assertHotBackend a
  | Right e <- eitherBackend a = Just e
  | otherwise = Nothing

takeTFS :: TorrentFileSystemEntryList st -> FilePath -> Maybe (TorrentFileSystemEntry st HotTorrent, TorrentFileSystemEntryList st)
takeTFS l p = fmap (\(t, l) -> (hotBackend' t, Map.fromList l)) $ takeTFS' (splitDirectories p) (Map.toList l)
  where
    takeTFS' :: (Typeable e) => [FilePath] -> [(FilePath, TorrentFileSystemEntry st e)] -> Maybe (Either (TorrentFileSystemEntry st StoredTorrent) (TorrentFileSystemEntry st HotTorrent), [(FilePath, TorrentFileSystemEntry st e)])
    takeTFS' [] _ = Nothing
    takeTFS' _ [] = Nothing
    takeTFS' path ((name, hot@(TFSUninitialized stored)) : ex) =
      let inner = takeTFS' path [(name, stored)]
          tail = takeTFS' path ex
          mergeUninitialized (Just (r, [])) t = Just (r, ex)
          mergeUninitialized Nothing (Just (m, t)) = Just (m, (name, hot) : t)
          mergeUninitialized Nothing Nothing = Nothing
          mergeUninitialized (Just (_, _ : _)) _ = error "Should not happen" -- TODO Handle other cases
       in mergeUninitialized inner tail
    takeTFS' [file] ((name, entry) : ex)
      | file == name = Just (eitherBackend entry, ex)
      | Just (res, rest) <- takeTFS' [file] ex = Just (res, (name, entry) : rest)
      | otherwise = Nothing
    takeTFS' path@(dir : px) ((name, entry) : ex)
      | name == dir,
        Just (x'@TFSDir {_contents = c}) <- gcast entry,
        Just (res, new) <- takeTFS' px (Map.toList c) =
          Just (res, (name, x' {_contents = Map.fromList new}) : ex)
      | Just (res, new) <- takeTFS' path ex = Just (res, (name, entry) : new)
      | otherwise = Nothing

getTFS' :: TorrentFileSystemEntryList st -> FilePath -> Maybe (TorrentFileSystemEntry st HotTorrent, [FilePath])
getTFS' files = getTFS'' [] (Just files) . splitDirectories
  where
    getTFS'' ((name, last) : rest) _ [] = Just (last, name : map fst rest)
    getTFS'' _ Nothing (_ : _) = Nothing
    getTFS'' walk (Just files) (dir : xs) = case Map.lookup dir files of
      Just m -> getTFS'' ((dir, m) : walk) (m ^? contents) xs
      Nothing -> Nothing

getTFS :: TorrentFileSystemEntryList st -> String -> [(FilePath, TorrentFileSystemEntry st HotTorrent)]
getTFS files dir = case getTFS' files dir of
  Just (TFSDir {_contents = c}, _name : _) -> Map.toList c
  -- Just (dir@TFSTorrentDir {}, name : _) -> Map.toList $ dir ^?! contents
  Just (file@TFSFile {}, name : _) -> [(name, file)]
  Just (file@TFSTorrentFile {}, name : _) -> [(name, file)]
  Nothing -> []
