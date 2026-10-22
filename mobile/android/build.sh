#!/bin/bash
#ChrysaLisp for Android. Builds the APK with Gradle, and with -r installs it
#on the device or emulator adb sees, and starts it.
#
#mobile/android/build.sh [-r]
#
#Wants the Android SDK, with the NDK and CMake 4.1.2 in it, and a JDK, that
#of Android Studio is used if JAVA_HOME is not set. The project can be
#opened in Android Studio as well, the folder mobile/android.

set -e
cd "$(dirname "$0")/.."
mobile=$(pwd)

export ANDROID_HOME="${ANDROID_HOME:-$HOME/Library/Android/sdk}"
if [ -z "$JAVA_HOME" ] && [ -d "/Applications/Android Studio.app/Contents/jbr/Contents/Home" ]
then
	export JAVA_HOME="/Applications/Android Studio.app/Contents/jbr/Contents/Home"
fi
echo "sdk.dir=$ANDROID_HOME" > android/local.properties

./fetch_sdl.sh
./bundle.sh build/assets/chrysalisp.tar

cd android
./gradlew --no-daemon assembleRelease
apk="$mobile/android/app/build/outputs/apk/release/app-release.apk"
echo "Built $apk"

if [ "$1" == "-r" ]
then
	adb="$ANDROID_HOME/platform-tools/adb"
	"$adb" install -r "$apk"
	"$adb" shell am start -n org.chrysalisp.app/.ChrysaLispActivity
fi
