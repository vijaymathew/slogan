INSTALL_DIR=/usr/bin
SLOGAN_ROOT=/usr/slogan
GSC=../platform/gsc/gsc/gsc

all:
	${GSC} -o slogan -exe ./prelude/*.scm repl.scm

install:
	cp slogan ${INSTALL_DIR}

uninstall:
	rm ${INSTALL_DIR}/slogan

clean:
	rm -f *.o*
	rm -f ./prelude/*.o*
	rm -f ./slogan

