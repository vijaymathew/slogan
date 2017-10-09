.PHONY: test

SLOGAN_ROOT ?= ../

INSTALL_DIR ?= /usr/local/bin
CONFIG_DIR ?= /etc/slogan

GSC=${SLOGAN_ROOT}/platform/gsc/gsc/gsc

LIBFFI_PATH=${SLOGAN_ROOT}/platform/libffi-3.2.1
LIBFFI_LIB=${LIBFFI_PATH}/target/usr/local/lib/libffi.a
LIBFFI_INC=${LIBFFI_PATH}/target/usr/local/lib/libffi-3.2.1/include

OPT_LIBS=-lffi
CORE_SFILES=$(wildcard core/*.scm)
CORE_CFILES = \
	core/array.c \
	core/a.c \
	core/bitvect.c \
	core/char.c \
	core/compiler.c \
	core/consts.c \
	core/extn.c \
	core/ffi.c \
	core/ilcodegen.c \
	core/libffi.c \
	core/list.c \
	core/lpair.c \
	core/match.c \
	core/ns.c \
	core/number.c \
	core/os.c \
	core/package.c \
	core/parallel.c \
	core/stream.c \
	core/string.c \
	core/syntax.c \
	core/table.c \
	core/task.c \
	core/tokenizer.c \
	core/util.c \
	core/z.c

CORE_OFILES = \
	core/array.o \
	core/a.o \
	core/bitvect.o \
	core/char.o \
	core/compiler.o \
	core/consts.o \
	core/extn.o \
	core/ffi.o \
	core/ilcodegen.o \
	core/libffi.o \
	core/list.o \
	core/lpair.o \
	core/match.o \
	core/ns.o \
	core/number.o \
	core/os.o \
	core/package.o \
	core/parallel.o \
	core/stream.o \
	core/string.o \
	core/syntax.o \
	core/table.o \
	core/task.o \
	core/tokenizer.o \
	core/util.o \
	core/z.o

all: slogan

slogan: $(CORE_SFILES) $(CORE_CFILES) $(CORE_OFILES) \
	core/_slogan.c core/_slogan.o libslogan.a
	${GSC} -o slogan -exe -l ./core/_slogan.c -ld-options "libslogan.a ${LIBFFI_LIB}" repl.scm

core/_slogan.c: $(CORE_CFILES)
	${GSC} -link -o $@ -preload $(CORE_CFILES)

core/_slogan.o: $(CORE_CFILES) core/_slogan.c
	${GSC} -cc-options "-D___LIBRARY" -obj -o ./core/_slogan.o ./core/_slogan.c

core/%.c: core/%.scm
	${GSC} -c -cc-options "-I${LIBFFI_INC}" $^

core/%.o: core/%.c
	${GSC} -obj -cc-options "-I${LIBFFI_INC}" $^

libslogan.a: $(CORE_OFILES) core/_slogan.o
	ar -rc $@ $^

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


