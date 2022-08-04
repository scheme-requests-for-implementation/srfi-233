.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs ini-files-test.scm

test-gauche:
	gosh -I . ini-files-test.scm

test-kawa:
	cp ini-files.sld ini-files.scm
	kawa ini-files-test.scm
	rm ini-files.scm

test-chibi:
	chibi-scheme ini-files-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o ini-files.so ini-files.sld
	csi -I . -R r7rs -s ini-files-test.scm
	rm ini-files.so
	rm ini-files.import.scm
