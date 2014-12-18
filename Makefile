.PHONY: build
build:
	cabal configure
	cabal build

.PHONY: mac
mac:
	cabal configure --flags LocalResources
	cabal build
	cp dist/build/jelly/jelly mac/jelly
	strip mac/jelly
	rm -rf mac/resources
	cp -r resources mac/resources
	rm -rf mac/libs
	dylibbundler -cd -of -b -x mac/jelly -d mac/libs -p "@executable_path/libs/"

.PHONY: mingw-deps
mingw-deps:
	cd mingw-deps && ./fetch.sh
	cabal install --only-dependencies --extra-lib-dirs=mingw-deps/lib --extra-include-dirs=mingw-deps/include --with-pkg-config=mingw-deps/bin/pkg-config.exe --allow-newer=sdl2

.PHONY: mingw
mingw:
	cabal configure --flags LocalResources
	cabal build
	cp dist/build/jelly/jelly.exe mingw/jelly.exe
	strip mingw/jelly.exe
	rm -rf mingw/*.dll
	cp mingw-deps/bin/*.dll mingw/
	rm -rf mingw/resources
	cp -r resources mingw/resources
