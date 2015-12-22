SLOGAN_HOME ?= ../
INSTALL_DIR=/usr/bin
SLOGAN_ROOT=/usr/slogan
GSC=${SLOGAN_HOME}/platform/gsc/gsc/gsc
LIBFFI_PATH=${SLOGAN_HOME}/platform/libffi-3.2.1
LIBFFI_LIB=${LIBFFI_PATH}/target/usr/local/lib
LIBFFI_INC=${LIBFFI_PATH}/target/usr/local/lib/libffi-3.2.1/include

all:
	${GSC} -o slogan -cc-options "-I${LIBFFI_INC}" -ld-options "-L${LIBFFI_LIB} -lffi" -exe ./prelude/*.scm repl.scm

install:
	cp slogan ${INSTALL_DIR}

uninstall:
	rm ${INSTALL_DIR}/slogan

clean:
	rm -f *.o*
	rm -f ./prelude/*.o*
	rm -f ./slogan

