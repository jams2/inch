debug: stst.s startup.c
	gcc -g -O0 -fomit-frame-pointer stst.s startup.c -o stst

.PHONY: debug
