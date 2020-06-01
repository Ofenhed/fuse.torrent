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

void delete_object_with_destructor(h_with_destructor* h, void *obj) {
  auto des = static_cast<std::function<void(void*)>*>(h->destructor);
  (*des)(obj);
  delete des;
  delete h;
}

const h_with_destructor *create_object_with_destructor(void* object, std::function<void(void*)> *destructor) {
  auto des = new h_with_destructor;
  des->object = object;
  des->destructor = destructor;
  return des;
}


const h_with_destructor *create_torrent_handle(const lt::sha1_hash &h) {
  auto hash = new lt::sha1_hash(h);
  auto destructor = new std::function<void(void*)>([] (void* obj) { delete static_cast<decltype(hash)>(obj); std::cerr << "Tried delete" << std::endl; });
  return create_object_with_destructor(hash, destructor);
}

void* init_torrent_session(char *savefile, void (*callback)()) {
  std::cerr << "Torrent session initialization" << std::endl;
  lt::settings_pack torrent_settings;
  torrent_settings.set_bool(lt::settings_pack::bool_types::enable_dht, true);
  torrent_settings.set_int(lt::settings_pack::int_types::out_enc_policy, lt::settings_pack::enc_policy::pe_forced);
  torrent_settings.set_int(lt::settings_pack::int_types::in_enc_policy, lt::settings_pack::enc_policy::pe_forced);
  torrent_settings.set_int(lt::settings_pack::int_types::seed_choking_algorithm, lt::settings_pack::seed_choking_algorithm_t::anti_leech);
  torrent_settings.set_int(lt::settings_pack::int_types::alert_mask, lt::alert::storage_notification | lt::alert::piece_progress_notification | lt::alert::status_notification);
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
  sess->session.set_alert_notify(std::function<void()>(callback));
  std::cerr << "Torrent session initialization completed" << std::endl;
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

const h_with_destructor* get_torrent(void *s, uint index) {
  auto *session = static_cast<torrent_session*>(s);
  auto torrents = session->session.get_torrents();
  try {
    return create_torrent_handle(torrents.at(index).info_hash());
  } catch (std::out_of_range e) {
    return NULL;
  }
}

const h_with_destructor *add_torrent(void* s, char* const magnet, char* const destination) {
  auto *session = static_cast<torrent_session*>(s);
  auto p = lt::parse_magnet_uri(magnet);
  std::ostringstream path;
  path << destination << "/" << p.info_hash;
  p.save_path = path.str();

  //p.flags &= ~lt::torrent_flags::auto_managed;
  p.flags |= lt::torrent_flags::upload_mode;
  session->session.async_add_torrent(std::move(p));
  return create_torrent_handle(p.info_hash);
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

const h_with_destructor *get_torrent_info(void *s, void *h) {
  auto *session = static_cast<torrent_session*>(s);
  auto *hash = static_cast<lt::sha1_hash*>(h);
  auto handle = session->session.find_torrent(*hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    if (status.has_metadata) {
      auto info = handle.torrent_file();
      session->last_torrent_filenames.clear();
      auto storage = info->files();
      std::vector<uint> file_size;
      for (auto file : storage.file_range()) {
        session->last_torrent_filenames.push_back(storage.file_path(file));
        file_size.push_back(storage.file_size(file));
        auto start = storage.map_file(file, 0, 0);
        auto end = storage.map_file(file, storage.file_size(file)-1, 0);
        std::cerr << "File " << storage.file_name(file) << " stretches from " << start.piece << "+" << start.start << " to " << end.piece << "+" << end.start;
      }
      uint num_files = session->last_torrent_filenames.size(); 
      auto ret = new torrent_files_info;
      ret->num_files = num_files;
      ret->files = new torrent_file_info[num_files];
      for (size_t i = 0; i < num_files; ++i) {
        ret->files[i].filename = session->last_torrent_filenames.at(i).c_str();
        ret->files[i].filesize = file_size.at(i);
      }
      return create_object_with_destructor(ret, new std::function<void(void*)>([](void* obj){
            auto de = static_cast<decltype(ret)>(obj);
            delete[] de->files;
            delete de;
            }));
    }
  }
  return NULL;
}

void* pop_alert(void* s) {
  auto *session = static_cast<torrent_session*>(s);
  if (!session->alert_queue.empty()) {
    session->alert_queue.erase(session->alert_queue.begin());
  }
  if (!session->alert_queue.empty()) {
    return session->alert_queue.at(0);
  }
  session->session.pop_alerts(&session->alert_queue);
  try {
    return session->alert_queue.at(0);
  } catch (const std::out_of_range&) {
    return NULL;
  }
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

const h_with_destructor *get_alert_torrent(void* a) {
  auto *alert = static_cast<lt::alert*>(a);
  if (auto torrent_alert = dynamic_cast<lt::torrent_alert*>(alert)) {
    std::cout << "Has torrent" << std::endl;
    return create_torrent_handle(torrent_alert->handle.info_hash());
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

