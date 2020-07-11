{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Torrent (module OldTorrent, withTorrentSession, requestSaveTorrentResumeData, setTorrentSessionActive, resumeTorrent, resetTorrent, checkTorrentHash, downloadTorrentParts) where

import OldTorrent
import TorrentTypes
import Control.Exception (bracket, finally)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Language.C.Types.Parse (cIdentifierFromString)
import Control.Monad (forM, (>=>))
import Data.Data (Data(..), Typeable(..))
import Data.Either (fromRight)
import Control.Concurrent.QSem (QSem, signalQSem)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Data.ByteString as B

data StdString

C.context $ C.cppCtx <> C.cppTypePairs [
  (fromRight (error "Type torrent_session does not exist") $ cIdentifierFromString True "std::string", [t| StdString |])
  ] <> C.bsCtx

C.include "<inttypes.h>"
C.include "<iostream>"
C.include "libtorrent_exports.hpp"
C.include "libtorrent/session.hpp"
C.include "libtorrent/extensions/smart_ban.hpp"
C.include "libtorrent/extensions/ut_metadata.hpp"
C.include "libtorrent/extensions/ut_pex.hpp"
C.include "libtorrent/torrent_flags.hpp"
C.include "libtorrent/read_resume_data.hpp"
C.include "libtorrent/torrent_info.hpp"
C.include "libtorrent/torrent_status.hpp"
C.include "libtorrent/magnet_uri.hpp"
C.include "libtorrent/bencode.hpp"

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
  let init_torrent_session = [C.block| void*
    {
      lt::settings_pack torrent_settings;
      torrent_settings.set_bool(lt::settings_pack::bool_types::enable_dht, true);
      torrent_settings.set_int(lt::settings_pack::int_types::out_enc_policy, lt::settings_pack::enc_policy::pe_forced);
      torrent_settings.set_int(lt::settings_pack::int_types::in_enc_policy, lt::settings_pack::enc_policy::pe_forced);
      torrent_settings.set_int(lt::settings_pack::int_types::seed_choking_algorithm, lt::settings_pack::seed_choking_algorithm_t::anti_leech);
      torrent_settings.set_int(lt::settings_pack::int_types::alert_mask, lt::alert::storage_notification | lt::alert::piece_progress_notification | lt::alert::status_notification);
      auto *sess = new torrent_session(torrent_settings);
      try {
        std::ifstream session_file($(char *csavefile));
        std::string session_raw;
        session_raw.assign((std::istreambuf_iterator<char>(session_file)), std::istreambuf_iterator<char>());
        auto decoded = lt::bdecode(session_raw);
        sess->session.load_state(decoded);
      } catch (...) {
        sess->session.add_dht_node(std::make_pair("router.utorrent.com", 6881));
        sess->session.add_dht_node(std::make_pair("router.bittorrent.com", 6881));
        sess->session.add_dht_node(std::make_pair("router.transmissionbt.com", 6881));
        sess->session.add_extension(&lt::create_ut_metadata_plugin);
        sess->session.add_extension(&lt::create_ut_pex_plugin);
        sess->session.add_extension(&lt::create_smart_ban_plugin);
        auto dht = sess->session.get_dht_settings();
        dht.privacy_lookups = true;
        sess->session.set_dht_settings(dht);
      }
      sess->session.set_alert_notify(std::function<void()>($(void(*callback)())));
      return static_cast<void*>(sess);
    } |]

      destroy_torrent_session ptr = [C.block| void
        {
          auto *session = static_cast<torrent_session*>($(void *ptr));
          try {
            std::ofstream session_file($(char *csavefile));
            auto it = std::ostream_iterator<char>(session_file);

            lt::entry encoded;
            session->session.save_state(encoded);
            lt::bencode(it, encoded);
          } catch (...) {
            delete session;
            throw;
          }
          delete session;
        } |]

  bracket (TorrentSession <$> init_torrent_session)
          (\(TorrentSession ptr) -> do
            destroy_torrent_session ptr
            freeHaskellFunPtr callback)
          runner

requestSaveTorrentResumeData :: TorrentSession -> IO CUInt
requestSaveTorrentResumeData (TorrentSession ptr) = [C.block| unsigned int
  {
    auto *csession = static_cast<torrent_session*>($(void *ptr));
    auto torrents = csession->session.get_torrents();
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
setTorrentSessionActive (TorrentSession ptr) active = let iactive = if active then 1 else 0
                                                        in [C.block| void
     {
       auto *session = static_cast<torrent_session*>($(void *ptr));
       if ($(int iactive)) {
         session->session.resume();
       } else {
         session->session.pause();
       }
     } |]

getTorrentHashLen = 160/8

addTorrent :: TorrentSession -> NewTorrentType -> FilePath -> IO (Maybe TorrentHandle)
addTorrent (TorrentSession ptr) (NewMagnetTorrent newMagnet) savedAt =
  withCString newMagnet $ \magnet ->
    withCString savedAt $ \destination -> do
      result <- [C.tryBlock| std::string*
        {
          auto *session = static_cast<torrent_session*>($(void *ptr));
          auto p = lt::parse_magnet_uri($(char *magnet));
          std::ostringstream path;
          path << $(char *destination) << "/" << p.info_hash;
          p.save_path = path.str();

          p.flags |= lt::torrent_flags::upload_mode;
          auto handle = new std::string(p.info_hash.to_string());
          session->session.async_add_torrent(std::move(p));
          return handle;
        } |]
      case result of
        Right h -> Just <$> finally (withStdString h peekTorrent')
                                    (free h)
        Left _ -> return Nothing

addTorrent (TorrentSession ptr) (NewTorrentFile newTorrent) savedAt =
  withCString savedAt $ \destination -> do
    result <- [C.tryBlock| std::string*
      {
        auto *session = static_cast<torrent_session*>($(void *ptr));
        lt::span<char const> torrent_data = {$bs-ptr:newTorrent, $bs-len:newTorrent};
        auto torrent_file = std::make_shared<lt::torrent_info>(torrent_data, lt::from_span);
        lt::add_torrent_params p;
        p.ti = torrent_file;
        std::ostringstream path;
        path << $(char *destination) << "/" << torrent_file->info_hash();
        p.save_path = path.str();

        p.flags |= lt::torrent_flags::upload_mode;
        auto handle = new std::string(torrent_file->info_hash().to_string());
        session->session.async_add_torrent(std::move(p));
        return handle;
      } |]
    case result of
      Right h -> Just <$> finally (withStdString h peekTorrent')
                                  (free h)
      Left _ -> return Nothing

resumeTorrent :: TorrentSession -> B.ByteString -> FilePath -> IO (Maybe TorrentHandle)
resumeTorrent (TorrentSession ptr) resumeData path =
  withCString path $ \cpath -> do
    result <- [C.tryBlock| std::string*
      {
        auto *session = static_cast<torrent_session*>($(void *ptr));
        auto p = lt::read_resume_data({$bs-ptr:resumeData, $bs-len:resumeData});
        session->session.async_add_torrent(std::move(p));
        return new std::string(p.info_hash.to_string());
      } |]
    case result of
      Right h -> Just <$> finally (withStdString h peekTorrent')
                                  (free h)
      Left _ -> return Nothing

resetTorrent :: TorrentSession -> TorrentHandle -> IO Bool
resetTorrent (TorrentSession ptr) torrent =
  withCString torrent $ \handle -> [C.block| int
    {
      auto *session = static_cast<torrent_session*>($(void *ptr));
      auto hash = lt::sha1_hash($(const char *handle));
      auto handle = session->session.find_torrent(hash);
      if (!handle.is_valid()) {
        return false;
      }
      auto status = handle.status();
      if (!status.has_metadata) {
        return false;
      }
      auto info = handle.torrent_file();
      std::vector<lt::download_priority_t> priorities(info->num_files(), lt::dont_download);
      handle.prioritize_files(priorities);
      handle.unset_flags(lt::torrent_flags::upload_mode);
      return true;
    } |] >>= \v -> return $ v /= 0

checkTorrentHash :: TorrentSession -> TorrentHandle -> IO ()
checkTorrentHash (TorrentSession ptr) torrent =
  withCString torrent $ \handle -> [C.block| void
    {
      auto *session = static_cast<torrent_session*>($(void *ptr));
      auto hash = lt::sha1_hash(static_cast<const char*>($(const char *handle)));
      auto handle = session->session.find_torrent(hash);
      auto status = handle.status();
      if (status.state != status.state_t::checking_files && status.state != status.state_t::checking_resume_data) {
        handle.force_recheck();
      }
    } |]

downloadTorrentParts :: TorrentSession -> TorrentHandle -> TorrentPieceType -> CUInt -> CUInt -> IO Bool
downloadTorrentParts (TorrentSession session) torrent part count timeout =
  withCString torrent $ \handle -> [C.block| int
    {
      auto *session = static_cast<torrent_session*>($(void *session));
      auto hash = lt::sha1_hash($(const char* handle));
      auto handle = session->session.find_torrent(hash);
      if (!handle.is_valid()) {
        return false;
      }
      auto status = handle.status();
      if (!status.has_metadata) {
        return false;
      }
      auto piece_index = $(int part);
      auto count = $(unsigned int count);
      auto timeout = $(unsigned int timeout);
      handle.set_piece_deadline(piece_index, 0, handle.alert_when_available);
      handle.resume();
      auto info = handle.torrent_file();
      auto num_pieces = info->num_pieces();
      if (piece_index + count >= num_pieces) {
        count = num_pieces - piece_index;
      }
      auto pieces_set = 0;
      for (auto i = 1; i < count; ++i) {
        if (!handle.have_piece(piece_index+i)) {
          ++pieces_set;
          handle.set_piece_deadline(piece_index+i, timeout * i);
        }
      }
      std::cerr << "Set priority for " << pieces_set << " pieces" << std::endl;
      return true;
    } |] >>= \v -> return $ v /= 0
