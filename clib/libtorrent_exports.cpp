#include "libtorrent_exports.h"

#include <cstdlib>
#include <iostream>
#include <chrono>
#include <sstream>
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



struct torrent_session {
  lt::session session;
  std::vector<lt::alert*> alert_queue;
  std::string last_torrent_filename;
  torrent_session(lt::settings_pack settings) : session(settings)
  {}
};

void* init_torrent_session() {
  std::cerr << "Torrent session initialization";
  lt::settings_pack torrent_settings;
  torrent_settings.set_bool(lt::settings_pack::bool_types::enable_dht, true);
  torrent_settings.set_int(lt::settings_pack::int_types::out_enc_policy, lt::settings_pack::enc_policy::pe_forced);
  torrent_settings.set_int(lt::settings_pack::int_types::in_enc_policy, lt::settings_pack::enc_policy::pe_forced);
  torrent_settings.set_int(lt::settings_pack::int_types::seed_choking_algorithm, lt::settings_pack::seed_choking_algorithm_t::anti_leech);
  auto *sess = new torrent_session(torrent_settings);
  sess->session.add_dht_node(std::make_pair("router.utorrent.com", 6881));
  sess->session.add_dht_node(std::make_pair("router.bittorrent.com", 6881));
  sess->session.add_dht_node(std::make_pair("router.transmissionbt.com", 6881));
  sess->session.add_extension(&lt::create_ut_metadata_plugin);
  sess->session.add_extension(&lt::create_ut_pex_plugin);
  sess->session.add_extension(&lt::create_smart_ban_plugin);
  auto dht = sess->session.get_dht_settings();
  dht.privacy_lookups = true;
  sess->session.set_dht_settings(dht);
  std::cerr << "DHT status: " << sess->session.is_dht_running() << std::endl;
  return static_cast<void*>(sess);
}

void destroy_torrent_session(void* s) {
  std::cerr << "Torrent session cleanup";
  delete static_cast<torrent_session*>(s);
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

const char* get_torrent_file(void *s, void *h, uint file_index) {
  auto *session = static_cast<torrent_session*>(s);
  auto *hash = static_cast<lt::sha1_hash*>(h);
  auto handle = session->session.find_torrent(*hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    if (status.has_metadata) {
      auto info = handle.torrent_file();
      auto files = info->files();
      session->last_torrent_filename = files.file_path(file_index);
      return session->last_torrent_filename.c_str();
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
