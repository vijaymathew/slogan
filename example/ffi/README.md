Before trying to build the shared library, please point the environment variable `SLOGAN_ROOT` to
the folder where you have cloned the Slogan repository.

To build the shared library on GNU/Linux run:

`gcc -Wall -shared -I$SLOGAN_ROOT/src/include -I$SLOGAN_ROOT/platform/gsc/include -fPIC -o clib.so clib.c`

and on Mac OS X:

`gcc -dynamiclib -I$SLOGAN_ROOT/src/include -I$SLOGAN_ROOT/platform/gsc/include -Wl,-undefined -Wl,dynamic_lookup -o clib.so clib.c`
