GSC=../platform/gsc/gsc/gsc
PLATFORM_INCS=../../platform/nanomsg/src
PLATFORM_LIBS=../platform/nanomsg/.libs

all:
	${GSC} -cc-options "-I${PLATFORM_INCS}" -o slogan -ld-options "-L${PLATFORM_LIBS} -lpthread -lnanomsg -lanl" -exe ./prelude/*.scm repl.scm 

clean:
	rm -f *.o*
	rm -f ./prelude/*.o*
	rm -f slogan

