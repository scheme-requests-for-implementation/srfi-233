.PHONY: test-gauche test-kawa test-chibi test-chicken

test-gauche:
	gosh -I . srfi-233-test.scm

test-kawa:
	cp srfi/233.sld srfi/233.scm
	kawa srfi-233-test.scm
	rm srfi/233.scm

test-chibi:
	chibi-scheme -I . srfi-233-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o srfi-233.so srfi/233.sld
	csi -I . -R r7rs -s srfi-233-test.scm
	rm srfi-233.so
	rm srfi-233.import.scm
