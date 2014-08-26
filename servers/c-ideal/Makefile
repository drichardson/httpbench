CFLAGS=-O3 -Wall -Werror -march=native -std=c99 -D_GNU_SOURCE -pthread
#CFLAGS=-O0 -g -Wall -Werror -march=native -std=c99 -D_GNU_SOURCE -pthread

SHARED_OBJS=list.o log.o
TARGETS=c-ideal c-saturate

all: $(TARGETS)

c-saturate: c-saturate-main.o $(SHARED_OBJS)
	$(CC) -pthread -lrt -o $@ $^

c-ideal: c-ideal-main.o $(SHARED_OBJS)
	$(CC) -pthread -o $@ $^

clean:
	$(RM) *.o $(TARGETS)

