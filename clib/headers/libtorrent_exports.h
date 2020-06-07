#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned int uint;

typedef void* h_destructor_type;

typedef struct {
  h_destructor_type destructor;
  void* object;
  void* c_private;
} h_with_destructor;

typedef struct {
  const char *filename;
  uint start_piece;
  uint start_piece_offset;
  uint filesize;
} torrent_file_info;

typedef struct {
  uint num_files;
  uint piece_size;
  const char *save_path;
  torrent_file_info *files;
} torrent_files_info;

typedef struct {
  int alert_type;
  const char* alert_what;
  int alert_category;
  const void* torrent;
  uint torrent_piece;
  const char* read_buffer;
  uint read_buffer_size;
} alert_type;

void* init_torrent_session(char *savefile, void (*callback)());
void destroy_torrent_session(char* savefile, void* s);

// Torrent
uint get_torrent_hash_len();
uint get_torrent_count(void *session);
const h_with_destructor *get_torrent(void *s, uint index);
const h_with_destructor *add_torrent(void *session, char *const filename, char *const path);
uint start_torrent(void *s, void* h);
uint download_torrent_parts(void* s, void* h, uint piece_index, uint count, uint timeout);
const char* get_torrent_name(void *s, void *h);
uint torrent_has_metadata(void *s, void *h);
const h_with_destructor *get_torrent_info(void *s, void *h);
const char* get_torrent_file(void *s, void *h, uint file_index);
uint get_torrent_num_files(void *s, void *h);

// Alert
const h_with_destructor* pop_alert(void* session);

// Helpers
void delete_object_with_destructor(h_with_destructor* h, void *_obj);

#ifdef __cplusplus
}
#endif
