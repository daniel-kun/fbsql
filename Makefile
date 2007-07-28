# Copyright 2007 Daniel Albuschat
# See license.txt for further licensing information

CXX=g++
CC=gcc
CFLAGS=-W -Wall -g
CXXFLAGS=-DIBPP_LINUX -DFBSQL_EXPORTS -D_CPLUSPLUS -W -Wall -g
OBJ=database.o error.o statement.o transaction.o service.o ibpp/core/all_in_one.o
LIB=-lfbclient
TEST_OBJ=test.o
TEST_LIB=-L. -lfbsql
TARGET=libfbsql.so
TEST_TARGET=test

all: ${OBJ} ${TEST_OBJ}
	${CXX} -shared -g -o ${TARGET} ${OBJ} ${LIB}
	${CC} -g -o ${TEST_TARGET} ${TEST_OBJ} ${TEST_LIB}

clean:
	@rm -vf *.o *.so
	@rm -vf ${TARGET} ${TEST_TARGET}
	@echo "Clean"
