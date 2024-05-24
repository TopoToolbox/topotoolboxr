#include <stdio.h>
#include <R.h>

const void test(){
	printf("hello world from the test.c \n"); // writing to stdout is not allowed on cran therfore a note on check() and reference to 'Writing R Extensions'
}
