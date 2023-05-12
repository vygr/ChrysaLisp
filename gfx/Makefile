# fbtest - test host GUI on SDL and framebuffer

#CFLAGS = -g -Wall -Werror
SDL_LIBS += -lSDL2

all: fbtest-sdl fbtest-fb

fb: fbtest-fb

fbtest-sdl: fbtest.o rom8x16.o gr-sdl.o
	$(CC) -o $@ $^ $(SDL_LIBS) $(LDLIBS)

fbtest-fb: fbtest.o rom8x16.o gr-fb.o
	$(CC) -o $@ $^ $(LDLIBS)

clean:
	rm -f *.o fbtest-sdl fbtest-fb
