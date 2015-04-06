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

Uses software variously licensed under the [GPL](http://www.gnu.org/licenses/gpl-3.0.html), [LGPL](http://www.gnu.org/licenses/lgpl.html), [MIT](http://opensource.org/licenses/MIT), and [BSD3](http://opensource.org/licenses/BSD-3-Clause) licenses.
The result is subject to the terms of the GPLv3.
Please [email](mailto:miketolly@gmail.com) if you need help obtaining source for any of the libraries.

Requires a recent version of libsndfile built from
[the Github repo](https://github.com/erikd/libsndfile).
Specifically it must be after
[this commit](https://github.com/erikd/libsndfile/commit/30b156a2a7d65d6b00fc2c443b6483d6231bef7e).

## Building standalone executable on Mac

  1. Build and install libsndfile from the repo link above.

  2. Install SDL2, SDL2_image, and Rubber Band.
    Homebrew works well.
    See [here](https://github.com/mtolly/rubberband/tree/master/homebrew)
    for a Rubber Band formula.

  3. Install the [Haskell Platform][] (or GHC, Cabal, Alex, and Happy), and C2HS.

  4. Install [dylibbundler](https://github.com/auriamg/macdylibbundler/).

  5. Run `make mac` to generate the complete distribution package
    in the `mac/` folder. This will install all Haskell dependencies,
    so `cabal sandbox init` beforehand if you want it sandboxed.

## Building standalone executable on Windows

  1. Install 32-bit [MinGW and MSYS](http://www.mingw.org/).

  2. Install the [Haskell Platform][] (or GHC, Cabal, Alex, and Happy).

  3. Install a recent version of `cabal-install` which supports `--allow-newer`.

  4. Install `c2hs`.

  5. Download [`winbox`](https://github.com/mtolly/winbox) and place in your `PATH`.

  6. Run `make mingw` to generate the complete
    distribution package in the `mingw` folder. This will download the
    needed MinGW packages, download and compile/install all the C
    libraries, download and install Python if you don't have it already
    (needed for compiling libsndfile), and finally install all Haskell
    dependencies, so `cabal sandbox init` beforehand if you want it
    sandboxed.

[Haskell Platform]: https://www.haskell.org/platform/
