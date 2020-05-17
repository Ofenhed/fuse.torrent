#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned int uint;

struct Alert {
  int timeout;
  char const* what;
  int type;
};

void* init_torrent_session();
void destroy_torrent_session(void* s);

// Torrent
const void* add_torrent(void *session, char *const filename, char *const path);
const char* get_torrent_name(void *s, void *h);
uint torrent_has_metadata(void *s, void *h);
const char* get_torrent_file(void *s, void *h, uint file_index);
uint get_torrent_num_files(void *s, void *h);
void get_torrent_info(void *session, void *hash);

// Alert
void* wait_for_alert(void* session, int timeout);
int get_alert_type(void* alert);
const char* get_alert_what(void* alert);
const char* get_alert_message(void* alert);
int get_alert_category(void* alert);

#ifdef __cplusplus
}
#endif
