{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Torrent
  ( TorrentAlert(..)
  , TorrentHandle()
  , TorrentInfo(..)
  , TorrentFile(..)
  , NewTorrentType(..)
  , withTorrentSession
  , getTorrentHash
  , findTorrent
  , addTorrent
  , requestSaveTorrentResumeData
  , setTorrentSessionActive
  , getTorrents
  , resumeTorrent
  , resetTorrent
  , getTorrentName
  , freeTorrentSpace
  , getTorrentFiles
  , checkTorrentHash
  , downloadTorrentParts
  , popAlerts
  ) where

import TorrentTypes
import InlineTypes
import Control.Exception (bracket, finally)
import Control.Comonad ((<<=))
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Marshal
import Foreign.Storable
import Language.C.Types.Parse (cIdentifierFromString)
import Control.Monad (forM, (>=>))
import Data.Data (Data(..), Typeable(..))
import Data.Either (fromRight)
import Control.Concurrent.QSem (QSem, signalQSem)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Data.ByteString as B
import qualified Data.Vector as V

data LtAlertPtr
data TorrentAlertHolder
data Sha1Hash

C.context $ C.cppCtx <> C.cppTypePairs
  [(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "std::string", [t| StdString |])
  ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "std::vector", [t| StdVector |])
  ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::session", [t| CTorrentSession |])
  ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::torrent_handle", [t| CTorrentHandle |])
  ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::alert", [t| LtAlert |])
  ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString True "lt::sha1_hash", [t| Sha1Hash |])
  ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString False "alert_type_holder", [t| TorrentAlertHolder |])
  ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString False "alert_type", [t| TorrentAlert |])
  ,(fromRight (error "Invalid type in cppTypePairs") $ cIdentifierFromString False "torrent_files_info", [t| TorrentInfo |])
  ] <> C.bsCtx

C.include "<inttypes.h>"
C.include "<iostream>"
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

deleteStdString :: Ptr StdString -> IO ()
deleteStdString ptr = [C.exp| void { delete $(std::string *ptr) } |]

withStdString :: Ptr StdString -> (CString -> IO a) -> IO a
withStdString str f = do
  ptr <- [C.exp| const char* { $(std::string *str)->c_str() } |]
  f ptr

withStdStringLen :: Ptr StdString -> (CStringLen -> IO a) -> IO a
withStdStringLen str f = do
  ptr <- [C.exp| const char* { $(std::string *str)->c_str() } |]
  len <- [C.exp| size_t { $(std::string *str)->length() } |]
  f (ptr, fromIntegral len)

withTorrentSession :: String -> QSem -> (TorrentSession -> IO a) -> IO a
withTorrentSession savefile sem runner = withCString savefile $ \csavefile -> do
  callback <- $(C.mkFunPtr [t| IO () |]) $ signalQSem sem
  let init_torrent_session = [C.block| lt::session*
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
      session->set_alert_notify(std::function<void()>($(void(*callback)())));
      return session;
    } |]

      destroy_torrent_session session = [C.block| void
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
        } |]

  bracket init_torrent_session
          (\ptr -> do
            destroy_torrent_session ptr
            freeHaskellFunPtr callback)
          runner

requestSaveTorrentResumeData :: TorrentSession -> IO CUInt
requestSaveTorrentResumeData session = [C.block| unsigned int
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
setTorrentSessionActive session active = let iactive = if active then 1 else 0
                                                        in [C.block| void
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
  torrents <- [C.block| std::vector<lt::torrent_handle*>*
    {
      auto *session = $(lt::session *session);
      auto torrents = session->get_torrents();
      auto handles = new std::vector<torrent_handle>;
      for (auto torrent : torrents) {
        handles->push_back(new lt::torrent_handle(std::move(torrent)));
      }
      return handles;
    } |]
  flip finally [C.exp| void { delete $(std::vector<lt::torrent_handle*>* torrents) } |] $ do
    let get_handle removeFirst = [C.block| lt::torrent_handle*
          {
            auto vec = $(std::vector<lt::torrent_handle*>* torrents);
            if ($(bool removeFirst)) { vec->pop_back(); }
            if (vec->empty()) { return NULL; }
            return vec->back();
          } |]
        get_all removeFirst = get_handle removeFirst >>= \unpacked ->
          if unpacked == nullPtr
             then return []
             else do
               translated <- wrapTorrentHandle unpacked
               rest <- get_all 1
               return $ translated:rest
    get_all 0

getTorrentHash :: TorrentHandle -> IO TorrentHash
getTorrentHash = flip withForeignPtr $ \torrent -> let sha1size = 20 in allocaBytes (fromIntegral sha1size) $ \buf -> [C.exp| void { memcpy($(char *buf), $(lt::torrent_handle *torrent)->info_hashes().get_best().data(), $(int sha1size)) } |] >> peekSha1' buf

findTorrent :: TorrentSession -> TorrentHash -> IO (Maybe TorrentHandle)
findTorrent session hash = do
  handle <- [C.block| lt::torrent_handle*
    {
      auto handle = $(lt::session *session)->find_torrent(lt::sha1_hash($bs-ptr:hash));
      if (handle.is_valid()) {
        return new lt::torrent_handle(std::move(handle));
      }
      return NULL;
    } |]
  if handle == nullPtr
     then return Nothing
     else Just <$> wrapTorrentHandle handle

addTorrent :: TorrentSession -> NewTorrentType -> FilePath -> IO (Maybe TorrentHash)
addTorrent session (NewMagnetTorrent newMagnet) savedAt =
  withCString newMagnet $ \magnet ->
    withCString savedAt $ \destination -> do
      result <- [C.tryBlock| std::string*
        {
          auto *session = $(lt::session *session);
          auto p = lt::parse_magnet_uri($(char *magnet));
          std::ostringstream path;
          path << $(char *destination) << "/" << p.info_hashes;
          p.save_path = path.str();

          p.flags |= lt::torrent_flags::upload_mode;
          auto handle = new std::string(p.info_hashes.get_best().to_string());
          session->async_add_torrent(std::move(p));
          return handle;
        } |]
      case result of
        Right h -> Just <$> finally (withStdString h peekSha1')
                                    (deleteStdString h)
        Left _ -> return Nothing

addTorrent session (NewTorrentFile newTorrent) savedAt =
  withCString savedAt $ \destination -> do
    result <- [C.tryBlock| std::string*
      {
        auto *session = $(lt::session *session);
        lt::span<char const> torrent_data = {$bs-ptr:newTorrent, $bs-len:newTorrent};
        auto torrent_file = std::make_shared<lt::torrent_info>(torrent_data, lt::from_span);
        lt::add_torrent_params p;
        p.ti = torrent_file;
        std::ostringstream path;
        path << $(char *destination) << "/" << torrent_file->info_hash();
        p.save_path = path.str();

        p.flags |= lt::torrent_flags::upload_mode;
        auto handle = new std::string(torrent_file->info_hashes().get_best().to_string());
        session->async_add_torrent(std::move(p));
        return handle;
      } |]
    case result of
      Right h -> Just <$> finally (withStdString h peekSha1')
                                  (deleteStdString h)
      Left _ -> return Nothing

resumeTorrent :: TorrentSession -> B.ByteString -> FilePath -> IO (Maybe TorrentHash)
resumeTorrent session resumeData path =
  withCString path $ \cpath -> do
    result <- [C.tryBlock| std::string*
      {
        auto *session = $(lt::session *session);
        auto p = lt::read_resume_data({$bs-ptr:resumeData, $bs-len:resumeData});
        session->async_add_torrent(std::move(p));
        return new std::string(p.info_hashes.get_best().to_string());
      } |]
    case result of
      Right h -> Just <$> finally (withStdString h peekSha1')
                                  (deleteStdString h)
      Left _ -> return Nothing

resetTorrent :: TorrentSession -> TorrentHandle -> IO Bool
resetTorrent session = flip withForeignPtr $ \torrent -> [C.block| int
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
      handle->unset_flags(lt::torrent_flags::upload_mode);
      handle->scrape_tracker();
      return true;
    } |] >>= \v -> return $ v /= 0

getTorrentName :: TorrentSession -> TorrentHandle -> IO (Maybe String)
getTorrentName session = flip withForeignPtr $ \torrent -> [C.block| const char*
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
    } |] >>= \res -> if res == nullPtr
                        then return Nothing
                        else Just <$> peekCString res

getTorrentFiles :: TorrentSession -> TorrentHandle -> IO (Maybe TorrentInfo)
getTorrentFiles session = flip withForeignPtr $ \torrent ->
    let create_infos = [C.block| torrent_files_info*
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
        destroy_infos ptr = [C.block| void
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
     in bracket create_infos destroy_infos $ \infos -> if infos == nullPtr
                                                          then return Nothing
                                                          else Just <$> peek infos


checkTorrentHash :: TorrentSession -> TorrentHandle -> IO ()
checkTorrentHash session = flip withForeignPtr $ \torrent -> [C.block| void
    {
      auto *session = $(lt::session *session);
      auto handle = $(lt::torrent_handle *torrent);
      auto status = handle->status();
      if (status.state != status.state_t::checking_files && status.state != status.state_t::checking_resume_data) {
        handle->force_recheck();
      }
    } |]

downloadTorrentParts :: TorrentSession -> TorrentHandle -> TorrentPieceType -> CUInt -> CUInt -> IO Bool
downloadTorrentParts session torrent part count timeout = withForeignPtr torrent $ \torrent -> [C.block| int
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
      auto piece_index = $(int part);
      auto count = $(unsigned int count);
      auto timeout = $(unsigned int timeout);
      handle->set_piece_deadline(piece_index, 0, lt::torrent_handle::alert_when_available);
      handle->resume();
      auto info = handle->torrent_file();
      auto num_pieces = info->num_pieces();
      if (piece_index + count >= num_pieces) {
        count = num_pieces - piece_index;
      }
      auto pieces_set = 0;
      for (auto i = 1; i < count; ++i) {
        if (!handle->have_piece(piece_index+i)) {
          ++pieces_set;
          handle->set_piece_deadline(piece_index+i, timeout * i);
        }
      }
      std::cerr << "Set priority for " << pieces_set << " pieces" << std::endl;
      return true;
    } |] >>= \v -> return $ v /= 0

popAlerts :: TorrentSession -> IO [TorrentAlert]
popAlerts session =
  let create_alerts = [C.block| std::vector<lt::alert*>*
        {
          auto *session = $(lt::session *session);
          auto *vector = new std::vector<lt::alert*>;
          session->pop_alerts(vector);
          return vector;
        } |]
      delete_alerts ptr = [C.exp| void { delete $(std::vector<lt::alert*>* ptr); } |]
    in bracket create_alerts delete_alerts $ \alerts ->
         let unpacker popFirst = [C.block| alert_type_holder*
               {
                 auto alerts = $(std::vector<lt::alert*>* alerts);
                 if ($(bool popFirst)) { alerts->erase(alerts->begin()); }
                 if (alerts->empty()) { return NULL; }
                 auto first_alert = alerts->begin();
                 auto alert = *first_alert;
                 auto response_holder = new alert_type_holder;
                 auto response = &response_holder->alert;
                 response->alert_type = alert->type();
                 response->alert_what = alert->what();
                 response->alert_category = alert->category();
                 response->torrent = NULL;
                 response->torrent_piece = 0;
                 response->read_buffer = NULL;
                 response->read_buffer_size = 0;
                 response->error_message = NULL;
                 response_holder->read_buffer_vector = NULL;
                 if (auto torrent_alert = dynamic_cast<lt::torrent_alert*>(alert)) {
                   // It seems the responder may not need the full torrent handle. If so, this may be optimized by returning a torrent hash instead.
                   response->torrent = new lt::torrent_handle(std::move(torrent_alert->handle));
                 }
                 if (auto read_piece_alert = lt::alert_cast<lt::read_piece_alert>(alert)) {
                   response->torrent_piece = read_piece_alert->piece;
                   response->read_buffer = read_piece_alert->buffer.get();
                   response->read_buffer_size = read_piece_alert->size;
                 }
                 if (auto save_resume_data_alert = lt::alert_cast<lt::save_resume_data_alert>(alert)) {
                   response_holder->read_buffer_vector = new std::vector<char>(lt::write_resume_data_buf(save_resume_data_alert->params));
                   response->read_buffer = response_holder->read_buffer_vector->data();
                   response->read_buffer_size = response_holder->read_buffer_vector->size();
                 }
                 if (auto scrape_failed_alert = lt::alert_cast<lt::scrape_failed_alert>(alert)) {
                   response->error_message = scrape_failed_alert->error_message();
                   std::cerr << "Scrape error message(" << scrape_failed_alert->error << "): " << response->error_message << std::endl;
                 }
                 return response_holder;
               } |]
             destructor ptr = [C.block| void
               {
                 auto response = $(alert_type_holder *ptr);
                 if (response == NULL) { return; }
                 delete response->read_buffer_vector;
                 delete response;
               } |]
             get_inner ptr = [C.exp| alert_type* { &$(alert_type_holder *ptr)->alert } |]
             unpack_all popFirst = bracket (unpacker popFirst) destructor $ \unpacked ->
               if unpacked == nullPtr
                  then return []
                  else do
                    translated <- get_inner unpacked >>= peek
                    rest <- unpack_all 1
                    return $ translated:rest
          in unpack_all 0
