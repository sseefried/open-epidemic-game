# build with sdl wrapper for mac osx (see mainc.c or hssdl/Examples/MacOSX)
PROGNAME=epidemic
VER=7.6.3
HCFLAGS=-package-db ./.cabal-sandbox/x86_64-osx-ghc-$(VER)-packages.conf.d -DSDL_BACKEND
OPTFLAGS=-O2
HC=ghc $(HCFLAGS)

OBJS=mainc.o MainSDL.o Game.o Graphics.o Backend/SDL.o

$(PROGNAME): $(OBJS)
	$(HC) $(OPTFLAGS) -no-hs-main $(OBJS) -o $@ -package SDL -package MonadRandom \
	                  -package cairo -static

mainc.o: mainc.c
	$(HC) -no-hs-main `sdl-config --cflags` -Wall $*.c -c

Game.o: Graphics.o

MainSDL.o: Game.o Backend/SDL.o

Backend/SDL.o: Game.o

%.o: %.hs
	$(HC) $(OPTFLAGS) -c $< -o $@

clean:
	rm -f Backend/*.hi Backend/*.o *.hi *.o *_stub.c *_stub.h $(PROGNAME)
.PHONY: clean
#
