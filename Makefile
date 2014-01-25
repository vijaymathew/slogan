all:
	make -C ./src

clean:
	make -C ./src clean
clean-all:
	cd ./platform/gsc
	make clean
	cd ../
	cd ./platform/nanomsg
	make clean
	cd ../
	make -C ./src clean
