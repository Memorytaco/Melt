
srcs = net.c
objs = net.o
lib = libnet.so

cflags = -Wall -Wextra -pedantic -shared -fPIC

.PHONY: clean default

default: $(lib)

$(lib): $(objs)
	$(CC) $(cflags) $^ -o $@

%.o: %.c
	$(CC) $(cflags) -c -o $@ $^

clean:
	rm -f *.so *.o
