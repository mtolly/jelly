.PHONY: build
build:
	cabal install --only-dependencies --allow-newer=sdl2
	cabal configure
	cabal build

.PHONY: mac
mac:
	cabal install --only-dependencies --allow-newer=sdl2 --flags LocalResources
	cabal configure --flags LocalResources
	cabal build
	cp dist/build/jelly/jelly mac/jelly
	strip mac/jelly
	rm -rf mac/resources
	cp -r resources mac/resources
	rm -rf mac/libs
	dylibbundler -cd -of -b -x mac/jelly -d mac/libs -p "@executable_path/libs/"
	cp README.md mac/

.PHONY: mingw-deps
mingw-deps:
	cd mingw-deps && ./fetch.sh
	cabal install c2hs

.PHONY: mingw
mingw:
	cabal install Cabal
	PATH="`pwd`/mingw-deps/bin:$$PATH" cabal install --only-dependencies --extra-lib-dirs="`pwd`/mingw-deps/lib" --extra-include-dirs="`pwd`/mingw-deps/include" --allow-newer=sdl2 --flags LocalResources
	PATH="`pwd`/mingw-deps/bin:$$PATH" cabal configure --flags LocalResources
	cabal build
	cp dist/build/jelly/jelly.exe mingw/jelly.exe
	strip mingw/jelly.exe
	rm -rf mingw/*.dll
	cp mingw-deps/bin/*.dll mingw/
	rm -rf mingw/resources
	cp -r resources mingw/resources
	cp /mingw/bin/libstdc++-6.dll mingw/
	cp README.md mingw/
