#define STR(a) XSTR(a)
#define XSTR(a) #a

#include <stdlib.h>
#include <HsFFI.h>

// We use the the widely-supported constructor/destructor gcc extension
// to initialize/deinitialize the Haskell RTS on shared-library load/unload.

static void library_init (void) __attribute__ ((constructor));
static void library_init(void)
{
  /* We fill in the library name in argv so it shows up in stacktraces and to make the GHCRTS envvar work. */
  static int argc = 1;
  static char *argv[] = { STR(MODULE) ".so", "+RTS", "-threaded", NULL };
  char **argv_ptr = argv;

  hs_init(&argc, &argv_ptr);
}

static void library_exit (void) __attribute__ ((destructor));
static void library_exit(void)
{
  hs_exit();
}
