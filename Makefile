all:
	make -C ./platform/gsc
	make -C ./platform/nanomsg
	make -C ./src

clean:
	make -C ./platform/gsc clean
	make -C ./platform/nanomsg clean
	make -C ./src
