A (very basic, work-in-progress) free viewer
for [Jammit](http://www.jammit.com/) content, written in Haskell. Built with:

  * [SDL2](https://www.libsdl.org/)
  * [`SDL2_image`](https://www.libsdl.org/projects/SDL_image/)
  * [OpenAL](http://en.wikipedia.org/wiki/OpenAL)
  * [`libsndfile`](http://www.mega-nerd.com/libsndfile/)
  * [Rubber Band Library](http://breakfastquay.com/rubberband/)
  * [`conduit`](https://hackage.haskell.org/package/conduit)

![Jelly](https://raw.github.com/mtolly/jelly/master/screenshot.png)

Supports loading multiple instruments for the same song all together.
Run like so:

    jelly dir1 [dir2 ...]

Each directory should be an instrument folder (one with an `info.plist` in it),
and they should all be for the same song.

Click on the buttons up top to play/pause, slow down, skip around,
and toggle each part's sheet music or audio.
