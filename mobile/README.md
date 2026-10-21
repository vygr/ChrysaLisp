# ChrysaLisp on iOS and Android

The apps are the GUI host program, `src/host`, with the SDL3 GUI and AUDIO
drivers, run in the VP64 emulator, `src/host/vp64.cpp`. Neither system lets an
app make code it can run, so there is no native boot image, and an app can not
start another process, so it is one node, the desktop, and it keeps to itself,
it does not join a mesh.

The files the system reads come with the app as one tar, which it unpacks to a
folder of its own the first time it starts, and again when the app has a new
one. That writes over the files of the tar, a file that is not one of them,
one the user made, is left as it is.

## What is needed

* The VP64 objects, all of them, not only the boot image of the snapshot:

	`./run_tui.sh -n 0 -f -s mobile/make_vp64.lisp`

	again after a change to a `.vp` file.

* The source of SDL3, 3.4 or later, `mobile/fetch_sdl.sh` puts it in
	`mobile/deps/SDL`. The build scripts do this themselves.

* iOS: Xcode and CMake.

* Android: the Android SDK, with the NDK and CMake 4.1.2 in it, and a JDK. The
	JDK of Android Studio is used if `JAVA_HOME` is not set.

## Building

iOS, for the simulator, and `-r` to install it on the one that is booted and
start it:

	mobile/ios/build.sh -r

for a device, signed by your team, or open the project in Xcode:

	mobile/ios/build.sh -d TEAM_ID
	open mobile/build/ios/ChrysaLisp.xcodeproj

Android, and `-r` to install it on the device or emulator adb sees and start
it:

	mobile/android/build.sh -r

The folder `mobile/android` can be opened in Android Studio as well, after one
run of the script, which makes the tar.

## Files

* `mobile_main.cpp`: the app's main, unpacks the tar, goes to its folder, and
	starts the host program, `main.cpp`'s main, with the emulator, `-e`.
* `bundle.sh`: makes the tar, the files of the system git knows of and the
	VP64 objects, `mobile/build/assets/chrysalisp.tar`.
* `CMakeLists.txt`: builds SDL3 and the host program, for both.
* `android_shm.cpp`: Android has no `shm_open`, these are files.
* `ios/`, `android/`: the projects of each.

## Using it

A finger is a pointer, as it is on a touch screen of a desktop. Three fingers
down at once show the keyboard of the screen, or hide it. On Android the GUI is
scaled by what the display says it is, as iOS does, so it is the size it would
be on a desktop.

## Not yet

* Only one node, so what is spread over nodes on a desktop runs on the one.
* The app goes on running when it is not in front, on iOS.
* The keyboard of the screen covers the bottom of the screen, nothing is moved
	up out from under it.
