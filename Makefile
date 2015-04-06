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

.PHONY: mingw
mingw:
	winbox init
	winbox install sdl2 sdl2-image openal libsndfile librubberband
	winbox cabal install --only-dependencies --allow-newer=sdl2 --flags LocalResources
	winbox cabal configure --flags LocalResources
	winbox cabal build
	cp dist/build/jelly/jelly.exe mingw/jelly.exe
	strip mingw/jelly.exe
	cp .winbox/bin/*.dll mingw/
	cp /mingw/bin/libstdc++-6.dll mingw/
	cp /mingw/bin/libgcc_s_dw2-1.dll mingw/
	cp -r resources mingw/resources
	cp README.md mingw/
