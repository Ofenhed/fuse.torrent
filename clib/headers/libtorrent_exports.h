#ifdef __cplusplus
extern "C" {
#endif

void* init_torrent_session();
void destroy_torrent_session(void* s);
void add_torrent(void* s, char* filename, char* path);

#ifdef __cplusplus
}
#endif
