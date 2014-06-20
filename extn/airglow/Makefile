all:
	${CC} -c -Wall -Werror -I$../../../platform/gsc/include -I$../../../src/include -fpic airglow.c 
	${CC} -shared -o libairglow.so airglow.o -lSDL2

clean:
	rm -f *.o
	rm -f *.so

