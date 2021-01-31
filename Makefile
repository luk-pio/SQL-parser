##
# AUG project - SQL pretty printer
#
# @file
# @version 0.1
#
CC = gcc
LEX = flex
YACC = bison
CFLAGS = -DYYDEBUG=1

sql: sql.tab.c sql.o
	${CC} -o $@ sql.tab.c sql.c

sql.tab.c sql.tab.h: sql.y
	${YACC} -vd sql.y

sql.c: sql.l
	${LEX} -o $@ $<

sql.o: sql.c sql.tab.h

test: sql in.txt
	./sql -d in.txt

clean:
	rm sql.tab.c sql.tab.h sql sql.c
