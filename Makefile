all:
	make -C ./platform/gsc
	make -C ./src

clean:
	make -C ./platform/gsc clean
	make -C ./src
