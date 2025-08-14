{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Torrent
  ( TorrentHandle (),
    TorrentInfo (..),
    TorrentMode (..),
    TorrentFile (..),
    NewTorrentType (..),
    withTorrentSession,
    getTorrentHash,
    findTorrent,
    addTorrent,
    requestSaveTorrentResumeData,
    setTorrentSessionActive,
    getTorrents,
    resumeTorrent,
    removeTorrent,
    resetTorrent,
    getTorrentName,
    getTorrentFiles,
    checkTorrentHash,
    downloadTorrentParts,
  )
where

import Control.Exception (bracket, finally)
import Data.Bits (toIntegralSized)
import qualified Data.ByteString as B
import Data.Coerce (coerce)
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import IntoOwned (IntoOwned (intoOwned))
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Interruptible as CI
import qualified Language.C.Inline.Unsafe as CU
import TorrentContext (torrentContext)
import TorrentTypes
import TorrentUtils (getBestHash, resolveInfoHash)
import Utils (withCStringCLen)

C.context $ torrentContext

C.include "<inttypes.h>"
C.include "<iostream>"
C.include "<mutex>"
C.include "<fstream>"
C.include "libtorrent/alert_types.hpp"
C.include "libtorrent/bencode.hpp"
C.include "libtorrent/download_priority.hpp"
C.include "libtorrent/entry.hpp"
C.include "libtorrent/extensions/smart_ban.hpp"
C.include "libtorrent/extensions/ut_metadata.hpp"
C.include "libtorrent/extensions/ut_pex.hpp"
C.include "libtorrent/magnet_uri.hpp"
C.include "libtorrent/read_resume_data.hpp"
C.include "libtorrent/session_params.hpp"
C.include "libtorrent/settings_pack.hpp"
C.include "libtorrent/torrent_flags.hpp"
C.include "libtorrent/torrent_handle.hpp"
C.include "libtorrent/torrent_info.hpp"
C.include "libtorrent/torrent_status.hpp"
C.include "libtorrent/write_resume_data.hpp"
C.include "libtorrent_exports.hpp"

withTorrentSession :: String -> (IO () -> TorrentSession -> IO a) -> IO a
withTorrentSession savefile runner = withCString savefile $ \csavefile -> do
  mutex <- [CU.exp| std::mutex* { new std::mutex } |] >>= newForeignPtr [C.funPtr| void deleteMutex(std::mutex *ptr) { delete ptr; } |]
  let wait_for_alert = withForeignPtr mutex $ \mutex' -> [CI.exp| void { $(std::mutex *mutex')->lock() }|]
  let init_torrent_session = withForeignPtr mutex $ \mutex' ->
        [CU.block| lt::session*
    {
      lt::settings_pack torrent_settings;
      torrent_settings.set_bool(lt::settings_pack::bool_types::enable_dht, true);
      torrent_settings.set_int(lt::settings_pack::int_types::out_enc_policy, lt::settings_pack::enc_policy::pe_forced);
      torrent_settings.set_int(lt::settings_pack::int_types::in_enc_policy, lt::settings_pack::enc_policy::pe_forced);
      torrent_settings.set_int(lt::settings_pack::int_types::seed_choking_algorithm, lt::settings_pack::seed_choking_algorithm_t::anti_leech);
      torrent_settings.set_int(lt::settings_pack::int_types::alert_mask, lt::alert::storage_notification | lt::alert::piece_progress_notification | lt::alert::status_notification);
      lt::session *session = NULL;
      try {
        std::ifstream session_file($(char *csavefile));
        std::string session_raw;
        session_raw.assign((std::istreambuf_iterator<char>(session_file)), std::istreambuf_iterator<char>());
        session = new lt::session(lt::read_session_params(session_raw));
      } catch (...) {
        session = new lt::session(std::move(torrent_settings));
        session->add_dht_node(std::make_pair("router.utorrent.com", 6881));
        session->add_dht_node(std::make_pair("router.bittorrent.com", 6881));
        session->add_dht_node(std::make_pair("router.transmissionbt.com", 6881));
        session->add_extension(&lt::create_ut_metadata_plugin);
        session->add_extension(&lt::create_ut_pex_plugin);
        session->add_extension(&lt::create_smart_ban_plugin);
        auto dht = session->get_dht_settings();
        dht.privacy_lookups = true;
        session->set_dht_settings(dht);
      }
      session->set_alert_notify(std::function<void()>([=] { $(std::mutex *mutex')->unlock(); }));
      return session;
    } |]

      destroy_torrent_session session = withForeignPtr mutex $ \mutex' ->
        [CU.block| void
        {
          auto *session = $(lt::session *session);
          try {
            std::ofstream session_file($(char *csavefile));
            auto it = std::ostream_iterator<char>(session_file);

            auto state = session->session_state();
            lt::entry encoded = lt::write_session_params(state);
            lt::bencode(it, encoded);
          } catch (...) {
            delete session;
            throw;
          }
          delete session;
          $(std::mutex *mutex')->unlock();
        } |]

  bracket
    init_torrent_session
    destroy_torrent_session
    $ runner wait_for_alert

requestSaveTorrentResumeData :: TorrentSession -> IO LTWord
requestSaveTorrentResumeData session =
  coerce
    <$> [CU.block| unsigned int
  {
    auto *session = $(lt::session *session);
    auto torrents = session->get_torrents();
    auto i = 0;
    for (auto torrent : torrents) {
      if (torrent.need_save_resume_data()) {
        torrent.save_resume_data();
        ++i;
      }
    }
    return i;
  } |]

setTorrentSessionActive :: TorrentSession -> Bool -> IO ()
setTorrentSessionActive session active =
  let iactive = fromBool active
   in [CU.block| void
     {
       auto *session = $(lt::session *session);
       if ($(int iactive)) {
         session->resume();
       } else {
         session->pause();
       }
     } |]

getTorrents :: TorrentSession -> IO [TorrentHandle]
getTorrents session = do
  torrents <-
    [CU.block| std::vector<lt::torrent_handle*>*
    {
      auto *session = $(lt::session *session);
      auto torrents = session->get_torrents();
      auto handles = new std::vector<lt::torrent_handle*>;
      for (auto torrent : torrents) {
        handles->push_back(new lt::torrent_handle(std::move(torrent)));
      }
      return handles;
    } |]
  flip finally [CU.exp| void { delete $(std::vector<lt::torrent_handle*>* torrents) } |] $ do
    let get_handle removeFirst =
          [CU.block| lt::torrent_handle*
          {
            auto vec = $(std::vector<lt::torrent_handle*>* torrents);
            if ($(bool removeFirst)) { vec->pop_back(); }
            if (vec->empty()) { return NULL; }
            return vec->back();
          } |]
        get_all removeFirst =
          get_handle removeFirst >>= \unpacked ->
            if unpacked == nullPtr
              then return []
              else do
                translated <- intoOwned unpacked
                rest <- get_all 1
                return $ translated : rest
    get_all 0

getTorrentHash :: TorrentHandle -> IO TorrentHash
getTorrentHash torrHandle = do
  let handler =
        [C.funPtr| lt::info_hash_t torrent_hash(lt::torrent_handle *torrent) {
      return torrent->info_hashes();
  } |]
  withForeignPtr torrHandle $ \torrent -> getBestHash handler torrent

-- deletedTorrentHash :: Ptr LtAlert -> IO TorrentHash
-- deletedTorrentHash alert = do
--  let handler = [C.funPtr| lt::info_hash_t deleted_torent_hash(lt::alert* alert) {
--      return static_cast<lt::torrent_deleted_alert*>(alert)->info_hash_t;
--    } |]
--  getBestHash handler alert

findTorrent :: TorrentSession -> TorrentHash -> IO (Maybe TorrentHandle)
findTorrent session hash = do
  handle <-
    [CU.block| lt::torrent_handle*
    {
      auto handle = $(lt::session *session)->find_torrent(lt::sha1_hash($bs-ptr:hash));
      if (handle.is_valid()) {
        return new lt::torrent_handle(std::move(handle));
      }
      return NULL;
    } |]
  if handle == nullPtr
    then return Nothing
    else Just <$> intoOwned handle

addTorrent :: TorrentSession -> NewTorrentType -> FilePath -> IO (Maybe TorrentHash)
addTorrent session (NewMagnetTorrent newMagnet) savedAt =
  withCStringCLen newMagnet $ \(magnet, magnetLen) ->
    withCStringCLen savedAt $ \(destination, destinationLen) -> do
      result <-
        [C.tryBlock| lt::info_hash_t*
        {
          auto *session = $(lt::session *session);
          auto p = lt::parse_magnet_uri(lt::string_view($(const char *magnet), $(size_t magnetLen)));

          p.flags |= lt::torrent_flags::upload_mode;
          auto info_hash = static_cast<lt::info_hash_t*>(malloc(sizeof(lt::info_hash_t)));
          *info_hash = p.info_hashes;
          p.save_path = std::string($(const char* destination), $(size_t destinationLen));
          session->async_add_torrent(std::move(p));
          return info_hash;
        } |]
      case result of
        Right h -> Just <$> resolveInfoHash h
        Left _ -> return Nothing
addTorrent session (NewTorrentFile newTorrent) savedAt =
  withCStringCLen savedAt $ \(destination, destinationLen) -> do
    result <-
      [C.tryBlock| lt::info_hash_t*
      {
        auto *session = $(lt::session *session);
        lt::span<char const> torrent_data = {$bs-ptr:newTorrent, $bs-len:newTorrent};
        auto torrent_file = std::make_shared<lt::torrent_info>(torrent_data, lt::from_span);
        lt::add_torrent_params p;
        p.ti = torrent_file;
        p.storage_mode = lt::storage_mode_sparse;

        p.flags |= lt::torrent_flags::upload_mode;
        auto info_hash = static_cast<lt::info_hash_t*>(malloc(sizeof(lt::info_hash_t)));
        *info_hash = torrent_file->info_hashes();
        p.save_path = std::string($(const char* destination), $(size_t destinationLen));
        session->async_add_torrent(std::move(p));
        return info_hash;
      } |]
    case result of
      Right h -> Just <$> resolveInfoHash h
      Left _ -> return Nothing

resumeTorrent :: TorrentSession -> B.ByteString -> IO (Maybe TorrentHash)
resumeTorrent session resumeData = do
  result <-
    [C.tryBlock| lt::info_hash_t*
    {
      auto *session = $(lt::session *session);
      auto p = lt::read_resume_data({$bs-ptr:resumeData, $bs-len:resumeData});
      auto info_hash = static_cast<lt::info_hash_t*>(malloc(sizeof(lt::info_hash_t)));
      *info_hash = p.info_hashes;
      session->async_add_torrent(std::move(p));
      return info_hash;
    } |]
  case result of
    Right h -> Just <$> resolveInfoHash h
    Left _ -> return Nothing

removeTorrent :: TorrentSession -> TorrentHandle -> IO Bool
removeTorrent session = flip withForeignPtr $ \torrent ->
  [CU.block| int
    {
      auto *session = $(lt::session *session);
      auto handle = $(lt::torrent_handle *torrent);
      if (!handle->is_valid()) {
        return false;
      }
      auto status = handle->status();
      if (!status.has_metadata) {
        return false;
      }
      session->remove_torrent(*handle, session->delete_files);
      return true;
    } |]
    >>= \v -> return $ v /= 0

data TorrentMode
  = TorrentUploadOnly
  | TorrentDownload

resetTorrent :: TorrentMode -> TorrentSession -> TorrentHandle -> IO Bool
resetTorrent mode session = flip withForeignPtr $ \torrent ->
  [CU.block| int
    {
      auto *session = $(lt::session *session);
      auto handle = $(lt::torrent_handle *torrent);
      if (!handle->is_valid()) {
        return false;
      }
      auto status = handle->status();
      if (!status.has_metadata) {
        return false;
      }
      auto info = handle->torrent_file();
      std::vector<lt::download_priority_t> priorities(info->num_files(), lt::dont_download);
      handle->prioritize_files(priorities);
      if ($(bool uploadMode)) {
        handle->set_flags(lt::torrent_flags::upload_mode);
      } else {
        handle->unset_flags(lt::torrent_flags::upload_mode);
      }
      handle->scrape_tracker();
      return true;
    } |]
    >>= \v -> return $ v /= 0
  where
    uploadMode
      | TorrentUploadOnly <- mode = CBool 1
      | otherwise = CBool 0

getTorrentName :: TorrentSession -> TorrentHandle -> IO (Maybe String)
getTorrentName session = flip withForeignPtr $ \torrent ->
  [CU.block| const char*
    {
      auto *session = $(lt::session *session);
      auto handle = $(lt::torrent_handle *torrent);
      if (handle->is_valid()) {
        auto status = handle->status();
        if (status.has_metadata) {
          auto info = handle->torrent_file();
          return info->name().c_str();
        }
      }
      return NULL;
    } |]
    >>= \res ->
      if res == nullPtr
        then return Nothing
        else Just <$> peekCString res

getTorrentFiles :: TorrentSession -> TorrentHandle -> IO (Maybe TorrentInfo)
getTorrentFiles session = flip withForeignPtr $ \torrent ->
  let create_infos =
        [CU.block| torrent_files_info*
          {
            auto *session = $(lt::session *session);
            auto handle = $(lt::torrent_handle *torrent);
            if (handle->is_valid()) {
              auto status = handle->status();
              if (status.has_metadata) {
                auto info = handle->torrent_file();
                auto storage = info->files();
                auto ret = new torrent_files_info;
                ret->num_files = storage.num_files();
                ret->save_path = strdup(handle->status(lt::torrent_handle::query_save_path).save_path.c_str());
                ret->piece_size = storage.piece_length();
                ret->files = new torrent_file_info[ret->num_files];
                for (auto file = 0; file < ret->num_files; ++file) {
                  auto path = storage.file_path(file);
                  ret->files[file].filename = strdup(path.c_str());
                  ret->files[file].filesize = storage.file_size(file);
                  auto start = storage.map_file(file, 0, 0);
                  ret->files[file].start_piece = start.piece;
                  ret->files[file].start_piece_offset = start.start;
                }
                return ret;
              }
            }
            return NULL;
          } |]
      destroy_infos ptr =
        [CU.block| void
          {
            auto de = $(torrent_files_info *ptr);
            if (de == NULL) { return; }
            free(const_cast<char*>(de->save_path));
            for (auto file = 0; file < de->num_files; ++file) {
              free(const_cast<char*>(de->files[file].filename));
            }
            delete[] de->files;
            delete de;
          } |]
   in bracket create_infos destroy_infos $ \infos ->
        if infos == nullPtr
          then return Nothing
          else Just <$> peek infos

checkTorrentHash :: TorrentSession -> TorrentHandle -> IO ()
checkTorrentHash session = flip withForeignPtr $ \torrent ->
  [CU.block| void
    {
      auto *session = $(lt::session *session);
      auto handle = $(lt::torrent_handle *torrent);
      auto status = handle->status();
      if (status.state != status.state_t::checking_files && status.state != status.state_t::checking_resume_data) {
        handle->force_recheck();
      }
    } |]

downloadTorrentParts :: TorrentSession -> TorrentHandle -> [TorrentPieceType] -> LTInt -> LTInt -> IO Bool
downloadTorrentParts _ _ [] _ _ = return True
downloadTorrentParts session torrent parts count timeout =
  withForeignPtr torrent $ \torrent' ->
    withArrayLen (coerce parts) $ \pieces_len pieces ->
      let pieces_len'
            | Just l <- toIntegralSized pieces_len = l
            | otherwise = undefined
          count' = coerce count
          timeout' = coerce timeout
       in toBool
            <$> [CU.block| int
                      {
                        auto *session = $(lt::session *session);
                        auto handle = $(lt::torrent_handle *torrent');
                        if (!handle->is_valid()) {
                          return false;
                        }
                        auto status = handle->status();
                        if (!status.has_metadata) {
                          return false;
                        }
                        auto pieces_set = 0;
                        auto pieces = $(int* pieces);
                        auto pieces_len = $(int pieces_len');
                        handle->clear_piece_deadlines();
                        handle->resume();
                        auto info = handle->torrent_file();
                        auto num_pieces = info->num_pieces();
                        while (pieces_len > 0) {
                          int second_smallest = pieces_len > 1 ? 1 : 0;
                          int smallest = 0;
                          for (int i = 1; i < pieces_len; ++i) {
                            if (pieces[i] < pieces[smallest]) {
                              second_smallest = smallest;
                              smallest = i;
                            } else if (pieces[i] < pieces[second_smallest]) {
                              second_smallest = i;
                            }
                          }
                          auto piece_index = pieces[smallest];
                          auto count = $(int count');
                          auto timeout = $(int timeout');
                          handle->set_piece_deadline(piece_index, 0, lt::torrent_handle::alert_when_available);
                          auto last = std::min(piece_index + count, num_pieces);
                          if (last > pieces[second_smallest] && second_smallest != smallest) {
                            last = pieces[second_smallest];
                          }
                          ++piece_index;
                          for (auto distance = 1; piece_index < last; ++piece_index, ++distance) {
                            handle->set_piece_deadline(piece_index, timeout * distance);
                          }
                          pieces[smallest] = pieces[0];
                          pieces += 1;
                          --pieces_len;
                        }
                        return true;
                      } |]
