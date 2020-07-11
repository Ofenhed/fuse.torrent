#include "headers/libtorrent_exports.h"
#include "headers/libtorrent_exports.hpp"

#include <chrono>
#include <functional>
#include <iostream>
#include <sstream>

#include "libtorrent/alert_types.hpp"
#include "libtorrent/bencode.hpp"
#include "libtorrent/download_priority.hpp"
#include "libtorrent/entry.hpp"
#include "libtorrent/extensions/smart_ban.hpp"
#include "libtorrent/extensions/ut_metadata.hpp"
#include "libtorrent/extensions/ut_pex.hpp"
#include "libtorrent/magnet_uri.hpp"
#include "libtorrent/read_resume_data.hpp"
#include "libtorrent/settings_pack.hpp"
#include "libtorrent/torrent_flags.hpp"
#include "libtorrent/torrent_info.hpp"
#include "libtorrent/torrent_status.hpp"
#include "libtorrent/write_resume_data.hpp"



void delete_object_with_destructor(h_with_destructor* h, void *obj) {
  auto des = static_cast<std::function<void(void*, void*)>*>(h->destructor);
  (*des)(obj, h->c_private);
  delete des;
  delete h;
}

const h_with_destructor *create_object_with_destructor(void* object, std::function<void(void*, void*)> *destructor, void* c_private = NULL) {
  auto des = new h_with_destructor;
  des->object = object;
  des->destructor = destructor;
  des->c_private = c_private;
  return des;
}

uint get_torrent_hash_len() {
  return 160/8;
}

const h_with_destructor *create_torrent_handle(const lt::sha1_hash &h) {
  const auto hash_size = get_torrent_hash_len();
  auto strhash = h.to_string();
  auto hash = malloc(hash_size);
  assert(h.size() == hash_size);
  memcpy(hash, h.data(), hash_size);
  auto destructor = new std::function<void(void*, void*)>([] (void* obj, void* _unused) { free(obj); });
  return create_object_with_destructor(hash, destructor);
}

void* init_torrent_session(char *savefile, void (*callback)()) {
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

void set_session_active(void *s, uint active) {
  auto *session = static_cast<torrent_session*>(s);
  if (active) {
    session->session.resume();
  } else {
    session->session.pause();
  }
}

uint save_torrents_resume_data(void *s) {
  auto *session = static_cast<torrent_session*>(s);
  auto torrents = session->session.get_torrents();
  auto i = 0;
  for (auto torrent : torrents) {
    if (torrent.need_save_resume_data()) {
      torrent.save_resume_data();
      ++i;
    }
  }
  return i;
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

const h_with_destructor *add_torrent_magnet(void* s, char* const magnet, char* const destination) try {
  auto *session = static_cast<torrent_session*>(s);
  auto p = lt::parse_magnet_uri(magnet);
  std::ostringstream path;
  path << destination << "/" << p.info_hash;
  p.save_path = path.str();

  p.flags |= lt::torrent_flags::upload_mode;
  auto handle = create_torrent_handle(p.info_hash);
  session->session.async_add_torrent(std::move(p));
  return handle;
} catch (...) { return NULL; }

const h_with_destructor *add_torrent_file(void* s, char* const file, uint file_len, char* const destination) try {
  auto *session = static_cast<torrent_session*>(s);
  lt::span<char const> torrent_data = {file, file_len};
  auto torrent_file = std::make_shared<lt::torrent_info>(torrent_data, lt::from_span);
  lt::add_torrent_params p;
  p.ti = torrent_file;
  std::ostringstream path;
  path << destination << "/" << torrent_file->info_hash();
  p.save_path = path.str();

  p.flags |= lt::torrent_flags::upload_mode;
  auto handle = create_torrent_handle(torrent_file->info_hash());
  session->session.async_add_torrent(std::move(p));
  return handle;
} catch (...) { return NULL; }

const h_with_destructor *resume_torrent(void* s, char* const data, uint data_len, char* const destination) try {
  auto *session = static_cast<torrent_session*>(s);
  auto p = lt::read_resume_data({data, data_len});
  session->session.async_add_torrent(std::move(p));
  return create_torrent_handle(p.info_hash);
} catch (...) { return NULL; }

uint reset_torrent(void *s, void* h) {
  auto *session = static_cast<torrent_session*>(s);
  auto hash = lt::sha1_hash(static_cast<const char*>(h));
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
}

void check_torrent_hash(void *s, void *h) {
  auto *session = static_cast<torrent_session*>(s);
  auto hash = lt::sha1_hash(static_cast<const char*>(h));
  auto handle = session->session.find_torrent(hash);
  auto status = handle.status();
  if (status.state != status.state_t::checking_files && status.state != status.state_t::checking_resume_data) {
    handle.force_recheck();
  }
}

uint download_torrent_parts(void* s, void* h, uint piece_index, uint count, uint timeout) {
  auto *session = static_cast<torrent_session*>(s);
  auto hash = lt::sha1_hash(static_cast<const char*>(h));
  auto handle = session->session.find_torrent(hash);
  if (!handle.is_valid()) {
    return false;
  }
  auto status = handle.status();
  if (!status.has_metadata) {
    return false;
  }
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
}

const char* get_torrent_name(void *s, void *h) {
  auto *session = static_cast<torrent_session*>(s);
  auto hash = lt::sha1_hash(static_cast<const char*>(h));
  auto handle = session->session.find_torrent(hash);
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
  auto hash = lt::sha1_hash(static_cast<const char*>(h));
  auto handle = session->session.find_torrent(hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    return status.has_metadata;
  }
  return false;
}

uint get_torrent_num_files(void *s, void *h) {
  auto *session = static_cast<torrent_session*>(s);
  auto hash = lt::sha1_hash(static_cast<const char*>(h));
  auto handle = session->session.find_torrent(hash);
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
  auto hash = lt::sha1_hash(static_cast<const char*>(h));
  auto handle = session->session.find_torrent(hash);
  if (handle.is_valid()) {
    auto status = handle.status();
    if (status.has_metadata) {
      auto info = handle.torrent_file();
      session->last_torrent_filenames.clear();
      auto storage = info->files();
      auto ret = new torrent_files_info;
      ret->num_files = storage.num_files();
      ret->save_path = strdup(handle.status(handle.query_save_path).save_path.c_str());
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
      return create_object_with_destructor(ret, new std::function<void(void*, void*)>([](void *obj, void *_unused){
            auto de = static_cast<decltype(ret)>(obj);
            free(const_cast<char*>(de->save_path));
            for (auto file = 0; file < de->num_files; ++file) {
              free(const_cast<char*>(de->files[file].filename));
            }
            delete[] de->files;
            delete de;
            }));
    }
  }
  return NULL;
}

void* pop_alert_internal(void* s) {
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

const h_with_destructor *pop_alert(void* s) {
  auto a = pop_alert_internal(s);
  if (a == NULL) {
    return NULL;
  }
  auto alert = static_cast<lt::alert*>(a);
  try {
    std::cerr << "Alert: " << alert->message() << "\n";
  } catch (...) {}
  auto response = new alert_type;
  response->alert_type = alert->type();
  response->alert_what = alert->what();
  response->alert_category = alert->category();
  auto destructors = new std::vector<const h_with_destructor*>;
  if (auto torrent_alert = dynamic_cast<lt::torrent_alert*>(alert)) {
    // std::cerr << "Has torrent " << torrent_alert->handle.info_hash() << std::endl;
    auto torrent_handle = create_torrent_handle(torrent_alert->handle.info_hash());
    response->torrent = static_cast<void*>(torrent_handle->object);
    destructors->push_back(torrent_handle);
  } else {
    response->torrent = NULL;
  }
  if (auto read_piece_alert = lt::alert_cast<lt::read_piece_alert>(alert)) {
    response->torrent_piece = read_piece_alert->piece;
    response->read_buffer = read_piece_alert->buffer.get();
    response->read_buffer_size = read_piece_alert->size;
  } else {
    response->torrent_piece = 0;
    response->read_buffer = NULL;
    response->read_buffer_size = 0;
  }
  if (auto save_resume_data_alert = lt::alert_cast<lt::save_resume_data_alert>(alert)) {
    auto vec = new std::vector<char>(lt::write_resume_data_buf(save_resume_data_alert->params));
    auto with_destructor = new h_with_destructor;
    with_destructor->object = vec;
    response->read_buffer = vec->data();
    response->read_buffer_size = vec->size();
    with_destructor->destructor = new std::function<void(void*, void*)>([](void *obj, void *_extra) {
      auto vec2 = static_cast<decltype(vec)>(obj);
      delete vec2;
    });
    destructors->push_back(with_destructor);
  }
  return create_object_with_destructor(response, new std::function<void(void*, void*)>([](void* obj, void* extra_destructors) {
    auto de = static_cast<decltype(response)>(obj);
    for (auto &destructor : *static_cast<decltype(destructors)>(extra_destructors)) {
      delete_object_with_destructor(const_cast<h_with_destructor*>(destructor), destructor->object);
    }
    delete de;
  }), destructors);
}
