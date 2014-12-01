app:
	cabal configure --flags MacApp \
	&& cabal build \
	&& cp dist/build/jelly/jelly Jelly.app/Contents/MacOS/jelly \
	&& rm -rf Jelly.app/Contents/Resources/img \
	&& cp -r img Jelly.app/Contents/Resources/img
