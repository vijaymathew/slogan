.PHONY: test

SLOGAN_ROOT ?= ../

INSTALL_DIR ?= /usr/local/bin
CONFIG_DIR ?= /etc/slogan

GSC=${SLOGAN_ROOT}/platform/gsc/gsc/gsc

LIBFFI_PATH=${SLOGAN_ROOT}/platform/libffi-3.2.1
LIBFFI_LIB=${LIBFFI_PATH}/target/usr/local/lib/libffi.a
LIBFFI_INC=${LIBFFI_PATH}/target/usr/local/lib/libffi-3.2.1/include

OPT_LIBS=-lffi

all:
	make clean
	${GSC} -c -cc-options "-I${LIBFFI_INC}" ./core/*.scm
	${GSC} -link -o ./core/_slogan.c -preload ./core/*.c
	${GSC} -obj -cc-options "-I${LIBFFI_INC}" ./core/*.c
	${GSC} -cc-options "-D___LIBRARY" -obj -o ./core/_slogan.o ./core/_slogan.c
	ar -rc libslogan.a ./core/*.o
	${GSC} -o slogan -exe -l ./core/_slogan.c -ld-options "${LIBFFI_LIB} libslogan.a" repl.scm


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

test:
	pwd
	rm -f ../example/*.scm
	rm -f ../example/*.o*
	rm -f ../test/test
	rm -f ../test/*.o*
	rm -f ../test/*.scm
	./slogan -x ../test/test
	../test/test
	rm -f ../example/*.scm
	rm -f ../example/*.o*
	rm -f ../test/test
	rm -f ../test/*.o*
	rm -f ../test/*.scm

clean:
	rm -f *.o*
	rm -f *.c
	rm -f ./core/*.o*
	rm -f ./core/*.c
	rm -f ./libslogan.a
	rm -f ./slogan
	rm -f ../example/*.scm
	rm -f ../example/*.o*
	rm -f ../test/test
	rm -f ../test/*.o*
	rm -f ../test/*.scm


