all:
	make -C ./src SLOGAN_HOME=`pwd`

install:
	make -C ./src install SLOGAN_HOME=`pwd`

uninstall:
	make -C ./src uninstall SLOGAN_HOME=`pwd`

clean:
	make -C ./src clean SLOGAN_HOME=`pwd`
