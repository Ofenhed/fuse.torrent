{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Torrent (module OldTorrent, withTorrentSession, requestSaveTorrentResumeData, setTorrentSessionActive) where

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
C.include "libtorrent_exports.hpp"
C.include "libtorrent/session.hpp"
C.include "libtorrent/extensions/smart_ban.hpp"
C.include "libtorrent/extensions/ut_metadata.hpp"
C.include "libtorrent/extensions/ut_pex.hpp"
C.include "libtorrent/torrent_flags.hpp"
C.include "libtorrent/read_resume_data.hpp"
C.include "libtorrent/torrent_info.hpp"
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
  let init_torrent_session = [C.block| void* {
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

      destroy_torrent_session ptr = [C.block| void{
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
requestSaveTorrentResumeData (TorrentSession ptr) = [C.block| unsigned int {
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
                                                        in [C.block| void {
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
      result <- [C.tryBlock| std::string* {
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
    result <- [C.tryBlock| std::string* {
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
    result <- [C.tryBlock| std::string* {
      auto *session = static_cast<torrent_session*>($(void *ptr));
      auto p = lt::read_resume_data({$bs-ptr:resumeData, $bs-len:resumeData});
      session->session.async_add_torrent(std::move(p));
      return new std::string(p.info_hash.to_string());
      } |]
    case result of
      Right h -> Just <$> finally (withStdString h peekTorrent')
                                  (free h)
      Left _ -> return Nothing
