A (very basic, work-in-progress) free viewer
for [Jammit](http://www.jammit.com/) content.
Written in Haskell; uses SDL2 and OpenAL.

Supports loading multiple instruments for the same song all together.
Run like so:

    jelly dir1 [dir2 ...]

Each directory should be an instrument folder (one with an `info.plist` in it),
and they should all be for the same song.

Click on the buttons up top to play/pause, slow down, skip around,
and toggle each part's sheet music or audio.
