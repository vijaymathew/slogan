SLOGAN_ROOT ?= ../

INSTALL_DIR=/usr/bin
CONFIG_DIR=/etc/slogan

GSC=${SLOGAN_ROOT}/platform/gsc/gsc/gsc

LIBFFI_PATH=${SLOGAN_ROOT}/platform/libffi-3.2.1
LIBFFI_LIB=${LIBFFI_PATH}/target/usr/local/lib
LIBFFI_INC=${LIBFFI_PATH}/target/usr/local/lib/libffi-3.2.1/include

OS := $(shell uname)
OPT_LIBS=""
ifeq ($(OS), Linux)
	OPT_LIBS=-lffi -lpthread -lrt
else
	OPT_LIBS=-lffi
endif

all:
	${GSC} -o slogan -cc-options "-I${LIBFFI_INC}" -ld-options "-L${LIBFFI_LIB} ${OPT_LIBS}" -exe ./prelude/*.scm repl.scm

install:
	cp slogan ${INSTALL_DIR}
	mkdir -p /usr/include/slogan
	mkdir -p ${CONFIG_DIR}
	rm -f ${CONFIG_DIR}/root
	touch ${CONFIG_DIR}/root
	echo ${SLOGAN_ROOT} > ${CONFIG_DIR}/root

uninstall:
	rm ${INSTALL_DIR}/slogan
	rm -rf ${CONFIG_DIR}

clean:
	rm -f *.o*
	rm -f ./prelude/*.o*
	rm -f ./slogan

