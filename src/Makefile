GSC = ../platform/gsc/gsc/gsc

all:
	${GSC} *.scm
	${GSC} ./prelude/*.scm
	${GSC} -o slogan -exe repl.scm

clean:
	rm *.o*
	rm ./prelude/*.o*
	rm slogan

