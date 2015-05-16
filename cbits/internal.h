#define assert(x) if (!(x)) { klog("assert failed " __FILE__":"); klog_hex(__LINE__); for(;;); }
#define printf(fmt,...) (void)0
#define die(x) (void)0
#define stderr NULL
#define fprintf(f, c, ...) (void)0

#define abort() do { klog("abort called " __FILE__ ":"); klog_hex(__LINE__); } while(0);

void *memset(void *ptr, int c, unsigned long n);
void *memcpy(void *dst, const void *src, unsigned long n);
