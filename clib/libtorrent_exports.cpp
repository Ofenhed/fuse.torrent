#include "headers/libtorrent_exports.h"

#include <cstdlib>
#include <iostream>
#include <chrono>
#include <sstream>
#include <functional>
#include "libtorrent/entry.hpp"
#include "libtorrent/bencode.hpp"
#include "libtorrent/session.hpp"
#include "libtorrent/torrent_info.hpp"
#include "libtorrent/torrent_status.hpp"
#include "libtorrent/torrent_flags.hpp"
#include "libtorrent/magnet_uri.hpp"
#include "libtorrent/extensions/ut_metadata.hpp"
#include "libtorrent/extensions/ut_pex.hpp"
#include "libtorrent/extensions/smart_ban.hpp"
#include "libtorrent/settings_pack.hpp"
#include "libtorrent/alert_types.hpp"



struct torrent_session {
  lt::session session;
  std::vector<lt::alert*> alert_queue;
  std::vector<std::string> last_torrent_filenames;
  torrent_session(lt::settings_pack settings) : session(settings)
  {}
};

void delete_object_with_destructor(h_with_destructor* h, void *_obj) {
  auto des = static_cast<std::function<void()>*>(h->destructor);
  (*des)();
  delete des;
  delete h;
}

const h_with_destructor *create_object_with_destructor(void* object, std::function<void()> *destructor) {
  auto des = new h_with_destructor;
  des->object = object;
  des->destructor = destructor;
  return des;
}


const void *create_torrent_handle(lt::sha1_hash &h) {
  auto hash = new lt::sha1_hash(h);
  auto destructor = new std::function<void()>([hash] { delete hash; std::cerr << "Tried delete" << std::endl; });
  return create_object_with_destructor(hash, destructor);
}

void* init_torrent_session(char *savefile) {
  std::cerr << "Torrent session initialization";
  lt::settings_pack torrent_settings;
  torrent_settings.set_bool(lt::settings_pack::bool_types::enable_dht, true);
  torrent_settings.set_int(lt::settings_pack::int_types::out_enc_policy, lt::settings_pack::enc_policy::pe_forced);
  torrent_settings.set_int(lt::settings_pack::int_types::in_enc_policy, lt::settings_pack::enc_policy::pe_forced);
  torrent_settings.set_int(lt::settings_pack::int_types::seed_choking_algorithm, lt::settings_pack::seed_choking_algorithm_t::anti_leech);
  torrent_settings.set_int(lt::settings_pack::int_types::alert_mask, lt::alert::error_notification | lt::alert::storage_notification | lt::alert::piece_progress_notification | lt::alert::status_notification);
  auto *sess = new torrent_session(torrent_settings);
  try {
    std::ifstream session_file(savefile);
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
  return static_cast<void*>(sess);
}

void destroy_torrent_session(char* savefile, void* s) {
  auto *session = static_cast<torrent_session*>(s);
  try {
    std::ofstream session_file(savefile);
    auto it = std::ostream_iterator<char>(session_file);

    lt::entry encoded;
    session->session.save_state(encoded);
    lt::bencode(it, encoded);
  } catch (...) {
    delete session;
    throw;
  }
  delete session;
}

uint get_torrent_count(void *s) {
  auto *session = static_cast<torrent_session*>(s);
  return session->session.get_torrents().size();
}

const void* get_torrent(void *s, uint index) {
  auto *session = static_cast<torrent_session*>(s);
  auto torrents = session->session.get_torrents();
  try {
    return static_cast<void*>(new lt::sha1_hash(torrents.at(index).info_hash()));
  } catch (std::out_of_range e) {
    return NULL;
  }
}

const void* add_torrent(void* s, char* const magnet, char* const destination) {
  auto *session = static_cast<torrent_session*>(s);
  auto p = lt::parse_magnet_uri(magnet);
  std::ostringstream path;
  path << destination << "/" << p.info_hash;
  p.save_path = path.str();

  //p.flags &= ~lt::torrent_flags::auto_managed;
  //p.flags |= lt::torrent_flags::upload_mode;
  session->session.async_add_torrent(std::move(p));
  return static_cast<void*>(new lt::sha1_hash(p.info_hash));
}

const char* get_torrent_name(void *s, void *h) {
  auto *session = static_cast<torrent_session*>(s);
  auto *hash = static_cast<lt::sha1_hash*>(h);
  auto handle = session->session.find_torrent(*hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    if (status.has_metadata) {
      auto info = handle.torrent_file();
      return info->name().c_str();
    }
  }
  return NULL;
}

uint torrent_has_metadata(void *s, void *h) {
  auto *session = static_cast<torrent_session*>(s);
  auto *hash = static_cast<lt::sha1_hash*>(h);
  auto handle = session->session.find_torrent(*hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    return status.has_metadata;
  }
  return false;
}

uint get_torrent_num_files(void *s, void *h) {
  auto *session = static_cast<torrent_session*>(s);
  auto *hash = static_cast<lt::sha1_hash*>(h);
  auto handle = session->session.find_torrent(*hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    if (status.has_metadata) {
      auto info = handle.torrent_file();
      return info->num_files();
    }
  }
  return 0;
}

const h_with_destructor *get_torrent_files(void *s, void *h) {
  auto *session = static_cast<torrent_session*>(s);
  auto *hash = static_cast<lt::sha1_hash*>(h);
  auto handle = session->session.find_torrent(*hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    if (status.has_metadata) {
      auto info = handle.torrent_file();
      session->last_torrent_filenames.clear();
      auto storage = info->files();
      for (auto file : storage.file_range()) {
        session->last_torrent_filenames.push_back(storage.file_path(file));
      }
      uint num_files = session->last_torrent_filenames.size(); 
      const char **ret = new const char*[num_files+1];
      for (size_t i = 0; i < num_files; ++i) {
        ret[i] = session->last_torrent_filenames.at(i).c_str();
      }
      ret[num_files] = NULL;
      return create_object_with_destructor(ret, new std::function<void()>([ret]{delete[] ret;}));
    }
  }
  return NULL;
}

void get_torrent_info(void* s, void* h) {
  auto *session = static_cast<torrent_session*>(s);
  auto *hash = static_cast<lt::sha1_hash*>(h);
  auto handle = session->session.find_torrent(*hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    if (status.has_metadata) {
      auto info = handle.torrent_file();
      auto files = info->files();
      std::cerr << "Found torrent with " << info->num_files() << std::endl;
      for (auto file : files.file_range()) {
        std::cerr << files.file_path(file) << std::endl;
      }
    } else {
      std::cerr << "Found torrent, but no metadata" << std::endl;
    }
  } else {
    std::cerr << "Could not find torrent" << std::endl;
  }
}

void* wait_for_alert(void* s, int timeout) {
  auto *session = static_cast<torrent_session*>(s);
  if (!session->alert_queue.empty()) {
    session->alert_queue.erase(session->alert_queue.begin());
  }
  if (!session->alert_queue.empty()) {
    return session->alert_queue.at(0);
  } else if (session->session.wait_for_alert(std::chrono::milliseconds(timeout)) != NULL) {
    session->session.pop_alerts(&session->alert_queue);
    return session->alert_queue.at(0);
  }
  return NULL;
}

int get_alert_type(void* s) {
  return static_cast<lt::alert*>(s)->type();
}

const char* get_alert_what(void* s) {
  return static_cast<lt::alert*>(s)->what();
}

int get_alert_category(void* s) {
  return static_cast<lt::alert*>(s)->category();
}

const void* get_alert_torrent(void* a) {
  auto *alert = static_cast<lt::alert*>(a);
  if (auto torrent_alert = lt::alert_cast<lt::torrent_alert>(alert)) {
    return static_cast<void*>(new lt::sha1_hash(torrent_alert->handle.info_hash()));
  }
  return NULL;
}

const void* get_alert_finished_piece(void* a) {
  auto *alert = static_cast<lt::alert*>(a);
  if (auto piece_alert = lt::alert_cast<lt::piece_finished_alert>(alert)) {
    return new lt::piece_index_t(piece_alert->piece_index);
  }
  return NULL;
}

