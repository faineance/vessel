#include <stdio.h>
#include "HsFFI.h"

// #define _GNU_SOURCE
#include <sched.h>

#ifdef __GLASGOW_HASKELL__
#include "Entrypoint_stub.h"
#endif

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);
  entrypoint();

  hs_exit();
  return 0;
}
