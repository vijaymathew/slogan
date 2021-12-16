// sample_c_lib.c

// Compile to a shared library (Linux):
// $ gcc -Wall -shared -fPIC -o sample_c_lib.so sample_c_lib.c

// Compile to a shared library (OS X):
// $ gcc -dynamiclib -Wl,-undefined -Wl,dynamic_lookup -o sample_c_lib.so sample_c_lib.c
#include <stdlib.h>

struct point {
   int x;
   int y;
};

struct point make_point(int x, int y)
{
   struct point p = {x, y};
   return p;
}

struct point scale_point(struct point p, int factor)
{
  struct point fp = {p.x * factor, p.y * factor};
  return fp;
}
