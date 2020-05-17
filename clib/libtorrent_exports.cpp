#include "libtorrent_exports.h"

#include <cstdlib>
#include <iostream>
#include "libtorrent/entry.hpp"
#include "libtorrent/bencode.hpp"
#include "libtorrent/session.hpp"
#include "libtorrent/torrent_info.hpp"
#include "libtorrent/torrent_flags.hpp"

void* init_torrent_session() {
  std::cerr << "Torrent session initialization";
  return static_cast<void*>(new lt::session);
}

void destroy_torrent_session(void* s) {
  std::cerr << "Torrent session cleanup";
  delete static_cast<lt::session*>(s);
}

void add_torrent(void* s, char* filename, char* destination) try {
  auto *session = static_cast<lt::session*>(s);
  lt::add_torrent_params p;
  p.save_path = std::string(destination);
  auto strfilename = std::string(filename);
  std::cerr << "reading " << strfilename;
  p.ti = std::make_shared<lt::torrent_info>(strfilename);

  // p.flags &= ^auto_managed & upload_mode;
  session->add_torrent(std::move(p));
} catch (boost::system::system_error e) {
  std::cerr << e.what() << filename;
  }
