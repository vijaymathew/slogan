To build the shared library on GNU/Linux run:

`gcc -Wall -shared -fPIC -o clib.so clib.c`

and on Mac OS X:

`gcc -dynamiclib -Wl,-undefined -Wl,dynamic_lookup -o clib.so clib.c`
