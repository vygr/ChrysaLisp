#!/bin/bash
#the source of SDL3, which the mobile builds compile with the app, into
#mobile/deps/SDL. The GUI driver wants 3.4 or later for its GPU renderer.

set -e
cd "$(dirname "$0")"
version="${1:-3.4.16}"
if [ -f deps/SDL/CMakeLists.txt ]
then
	echo "deps/SDL is there, delete it to fetch again"
	exit 0
fi
mkdir -p deps
git clone --depth 1 -b "release-$version" https://github.com/libsdl-org/SDL.git deps/SDL
