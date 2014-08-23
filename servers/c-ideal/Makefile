CFLAGS=-O2 -Wall -Werror -march=native -std=c99 -D_GNU_SOURCE

OBJS=c-ideal-main.o list.o log.o

c-ideal: $(OBJS)
	$(CC) -o $@ $(OBJS)

clean:
	$(RM) $(OBJS) c-ideal

