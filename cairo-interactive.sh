DIR=dist/build/Epidemic/Epidemic-tmp/src

ln -s /System/Library/Frameworks/Foundation.framework/Foundation $DIR/Foundation.o

cd src
ghci ../$DIR/util.o ../$DIR/Foundation.o ../$DIR/freetype_util.o \
     ../$DIR/objc_util.o  CairoInteractive.hs