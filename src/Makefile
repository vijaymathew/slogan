GSC = ../platform/gsc/gsc/gsc

all:
	${GSC} *.ss
	${GSC} ./prelude/*.ss
	${GSC} -o slogan -exe repl.ss

clean:
	rm *.o*
	rm ./prelude/*.o*
	rm slogan

