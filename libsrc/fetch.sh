#!/bin/bash
set -e
set -u

# Downloads source for all libraries that might be distributed with Jelly.

# Basic C libs
wget -nc https://www.libsdl.org/release/SDL2-2.0.3.tar.gz
# SDL_image also includes tiff, jpeg, png
wget -nc https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.0.tar.gz
wget -nc http://code.breakfastquay.com/attachments/download/34/rubberband-1.8.1.tar.bz2
wget -O libsndfile-eea13650b9.zip https://github.com/erikd/libsndfile/archive/eea13650b97611b0ab11904dd661de8bfb393a9c.zip

# Used on Windows
wget -nc http://kcat.strangesoft.net/openal-releases/openal-soft-1.16.0.tar.bz2
wget -nc http://www.mega-nerd.com/SRC/libsamplerate-0.1.8.tar.gz
wget -nc ftp://sourceware.org/pub/pthreads-win32/pthreads-w32-2-9-1-release.tar.gz
wget -nc http://www.fftw.org/fftw-3.3.4.tar.gz
wget -nc http://zlib.net/zlib-1.2.8.tar.gz

# Required if libsndfile was compiled with them (not actually used)
wget -nc http://downloads.xiph.org/releases/flac/flac-1.3.0.tar.xz
wget -nc http://downloads.xiph.org/releases/ogg/libogg-1.3.2.tar.xz
wget -nc http://downloads.xiph.org/releases/vorbis/libvorbis-1.3.4.tar.xz
