// gcc -c -Wall -Werror -I../platform/gsc/include -I../src/include -fpic egdlib.c 
// gcc -shared -o libegdlib.so egdlib.o

#include <stdio.h>
#include "slogan.h"
 
void c_hello (void)
{
  printf ("hello from C!\n");
}

void c_add_ten (___SLOGAN_OBJ i)
{
  printf ("%d\n", ___INT (i) + 10);
}

