#include <stdio.h>
#include "HsFFI.h"

// #define _GNU_SOURCE
#include <sched.h>

#ifdef __GLASGOW_HASKELL__

#endif

int main(int argc, char *argv[])
{
  printf("asd\n" );
  hs_init(&argc, &argv);

  hs_exit();
  return 0;
}
