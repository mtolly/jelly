build:
	cabal configure
	cabal build

app:
	cabal configure --flags MacApp
	cabal build
	strip dist/build/jelly/jelly
	cp dist/build/jelly/jelly Jelly.app/Contents/MacOS/jelly
	rm -rf Jelly.app/Contents/Resources/img
	cp -r img Jelly.app/Contents/Resources/img
	rm -rf Jelly.app/Contents/libs/*.dylib
	dylibbundler -cd -of -b -x Jelly.app/Contents/MacOS/jelly -d Jelly.app/Contents/libs/
