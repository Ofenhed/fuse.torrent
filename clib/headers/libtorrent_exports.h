#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned int uint;

typedef void* h_destructor_type;

typedef struct {
  h_destructor_type destructor;
  void* object;
} h_with_destructor;

void* init_torrent_session(char *savefile);
void destroy_torrent_session(char* savefile, void* s);

// Torrent
uint get_torrent_count(void *session);
const void* get_torrent(void *s, uint index);
const void* add_torrent(void *session, char *const filename, char *const path);
const char* get_torrent_name(void *s, void *h);
uint torrent_has_metadata(void *s, void *h);
const h_with_destructor *get_torrent_files(void *s, void *h);
const char* get_torrent_file(void *s, void *h, uint file_index);
uint get_torrent_num_files(void *s, void *h);
void get_torrent_info(void *session, void *hash);

// Alert
void* wait_for_alert(void* session, int timeout);
int get_alert_type(void* alert);
const char* get_alert_what(void* alert);
const char* get_alert_message(void* alert);
int get_alert_category(void* alert);

// Helpers
void delete_object_with_destructor(h_with_destructor* h, void *_obj);

#ifdef __cplusplus
}
#endif
