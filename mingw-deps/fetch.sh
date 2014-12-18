#!/bin/bash
set -e
set -u

# Prereq: install MinGW+MSYS
# From http://sourceforge.net/projects/mingw/files/Installer/
# download http://sourceforge.net/projects/mingw/files/Installer/mingw-get-setup.exe/download

# First do this manually
# C:\mingw\bin\mingw-get install mingw32-base msys-base
# Then run this script from msys.bat

# Make sure all MinGW programs are accessible
set +e # Turn off error checking temporarily
mount c:/mingw /mingw
set -e

# Install the rest of the MinGW stuff we'll need
mingw-get install mingw32-gcc-g++ mingw32-autotools msys-autogen msys-wget msys-unzip

if command -v python; then
  echo "Python is already installed."
else
  # From https://www.python.org/downloads/windows/
  wget -nc https://www.python.org/ftp/python/3.4.2/python-3.4.2.msi --no-check-certificate
  echo "Installing Python. Make sure to add Python to the PATH."
  msiexec -i python-3.4.2.msi
  echo "Python installed. Please restart your shell."
  exit 1
fi

# Make the lib folders
mkdir -p bin lib include
set +u # In case some vars doesn't exist
PATH="`pwd`/bin:$PATH"
PKG_CONFIG_PATH="`pwd`/lib/pkgconfig:$PKG_CONFIG_PATH"
ACLOCAL_PATH="`pwd`/share/aclocal:$ACLOCAL_PATH"
C_INCLUDE_PATH="`pwd`/include:$C_INCLUDE_PATH"
LD_LIBRARY_PATH="`pwd`/lib:$LD_LIBRARY_PATH"
set -u
DEPSDIR=`pwd`

# Install pkg-config
wget -nc http://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/pkg-config_0.26-1_win32.zip
wget -nc http://ftp.gnome.org/pub/gnome/binaries/win32/glib/2.28/glib_2.28.8-1_win32.zip
wget -nc http://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/gettext-runtime_0.18.1.1-2_win32.zip
unzip -o pkg-config_0.26-1_win32.zip
unzip -o glib_2.28.8-1_win32.zip
unzip -o gettext-runtime_0.18.1.1-2_win32.zip
wget -nc http://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/pkg-config-dev_0.26-1_win32.zip
unzip -o pkg-config-dev_0.26-1_win32.zip

# From http://libsdl.org/download-2.0.php
wget -nc http://libsdl.org/release/SDL2-devel-2.0.3-mingw.tar.gz
tar -xvzf SDL2-devel-2.0.3-mingw.tar.gz
cp -R SDL2-2.0.3/i686-w64-mingw32/* .
cp -R SDL2-2.0.3/include/* include/
cp -R SDL2-2.0.3/lib/x86/* lib/

# From https://www.libsdl.org/projects/SDL_image/
wget -nc https://www.libsdl.org/projects/SDL_image/release/SDL2_image-devel-2.0.0-mingw.tar.gz --no-check-certificate
tar -xvzf SDL2_image-devel-2.0.0-mingw.tar.gz
cp -R SDL2_image-2.0.0/i686-w64-mingw32/* .
# patch for a bug in SDL 2.0.3
wget -nc https://hg.libsdl.org/SDL/raw-file/e217ed463f25/include/SDL_platform.h --no-check-certificate
cp SDL_platform.h include/SDL2/

# From https://www.libsdl.org/projects/SDL_ttf/
wget -nc https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-devel-2.0.12-mingw.tar.gz --no-check-certificate
tar -xvzf SDL2_ttf-devel-2.0.12-mingw.tar.gz
cp -R SDL2_ttf-2.0.12/i686-w64-mingw32/* .

# From http://kcat.strangesoft.net/openal.html
wget -nc http://kcat.strangesoft.net/openal-soft-1.16.0-bin.zip
unzip -o openal-soft-1.16.0-bin.zip
cp -R openal-soft-1.16.0-bin/bin/Win32/soft_oal.dll bin/OpenAL32.dll
cp -R openal-soft-1.16.0-bin/include/* include/
cp -R openal-soft-1.16.0-bin/libs/Win32/* lib/

# TODO: this shouldn't be necessary. we set ACLOCAL_PATH above
cp share/aclocal/pkg.m4 /mingw/share/aclocal
if [ -d libsndfile ]; then
  cd libsndfile
  git pull
else
  git clone https://github.com/erikd/libsndfile
  cd libsndfile
fi
./autogen.sh
./configure --prefix="$DEPSDIR"
make
make install
cd ..

wget -nc http://www.fftw.org/fftw-3.3.4.tar.gz
tar -xvzf fftw-3.3.4.tar.gz
cd fftw-3.3.4
./configure --prefix="$DEPSDIR"
make
make install
cd ..

wget -nc http://www.mega-nerd.com/SRC/libsamplerate-0.1.8.tar.gz
tar -xvzf libsamplerate-0.1.8.tar.gz
cd libsamplerate-0.1.8
./configure --prefix="$DEPSDIR"
make
make install
cd ..

wget -nc http://code.soundsoftware.ac.uk/attachments/download/690/vamp-plugin-sdk-2.5.tar.gz --no-check-certificate
tar -xvzf vamp-plugin-sdk-2.5.tar.gz
cd vamp-plugin-sdk-2.5
./configure --prefix="$DEPSDIR"
make
make install
cd ..

wget http://www.ladspa.org/ladspa_sdk/ladspa.h.txt -O include/ladspa.h

# from https://www.sourceware.org/pthreads-win32/
wget -nc ftp://sourceware.org/pub/pthreads-win32/pthreads-w32-2-9-1-release.zip
unzip -o pthreads-w32-2-9-1-release.zip
cp -R Pre-built.2/dll/x86/* bin/
cp -R Pre-built.2/include/* include/
cp -R Pre-built.2/lib/x86/* lib/
cp lib/libpthreadGC2.a lib/libpthread.a

wget -nc http://code.breakfastquay.com/attachments/download/34/rubberband-1.8.1.tar.bz2
tar -xvjf rubberband-1.8.1.tar.bz2
cd rubberband-1.8.1
./configure --prefix="$DEPSDIR"
sed -i 's/\.so/\.dll/' Makefile
make
make install
cd ..
rm lib/librubberband.a
cp lib/librubberband.dll bin/librubberband.dll
