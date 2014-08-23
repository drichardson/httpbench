CFLAGS=-O3 -Wall -Werror -march=native -std=c99 -D_GNU_SOURCE -pthread
#CFLAGS=-O0 -g -Wall -Werror -march=native -std=c99 -D_GNU_SOURCE -pthread

OBJS=c-ideal-main.o list.o log.o

c-ideal: $(OBJS)
	$(CC) -pthread -o $@ $(OBJS)

clean:
	$(RM) $(OBJS) c-ideal

