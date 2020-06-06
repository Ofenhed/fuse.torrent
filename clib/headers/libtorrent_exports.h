#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned int uint;

typedef void* h_destructor_type;

typedef struct {
  h_destructor_type destructor;
  void* object;
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

void* init_torrent_session(char *savefile, void (*callback)());
void destroy_torrent_session(char* savefile, void* s);

// Torrent
uint get_torrent_hash_len();
uint get_torrent_count(void *session);
const h_with_destructor *get_torrent(void *s, uint index);
const h_with_destructor *add_torrent(void *session, char *const filename, char *const path);
const char* get_torrent_name(void *s, void *h);
uint torrent_has_metadata(void *s, void *h);
const h_with_destructor *get_torrent_info(void *s, void *h);
const char* get_torrent_file(void *s, void *h, uint file_index);
uint get_torrent_num_files(void *s, void *h);

// Alert
void* pop_alert(void* session);
int get_alert_type(void* alert);
const char* get_alert_what(void* alert);
const char* get_alert_message(void* alert);
int get_alert_category(void* alert);
const h_with_destructor *get_alert_torrent(void* a);
const void* get_alert_finished_piece(void* a);

// Helpers
void delete_object_with_destructor(h_with_destructor* h, void *_obj);

#ifdef __cplusplus
}
#endif
