# Mobile how-to

ChrysaLisp on iOS and Android is the GUI host program run in the VP64
emulator, as one node. See `README.md` for how it is put together.

## Changes to the main source

Only the SDL3 GUI driver changed. A desktop build is the same as before:
everything new is behind `SDL_PLATFORM_IOS` / `SDL_PLATFORM_ANDROID`, or is a
scale that stays 1.

### `src/host/gui_sdl3.cpp`, `host_gui_init()`

On a phone or tablet the window is made fullscreen. The GUI is then told the
window's real size, whatever size it asked for, as the bare display path
already does:

```cpp
window = SDL_CreateWindow("ChrysaLisp GUI Window", rect->w, rect->h,
	SDL_WINDOW_RESIZABLE | ... 
#if defined(SDL_PLATFORM_IOS) || defined(SDL_PLATFORM_ANDROID)
	| SDL_WINDOW_FULLSCREEN
#endif
	);
#if defined(SDL_PLATFORM_IOS) || defined(SDL_PLATFORM_ANDROID)
if (window)
{
#if defined(SDL_PLATFORM_ANDROID)
	auto scale = SDL_GetDisplayContentScale(SDL_GetDisplayForWindow(window));
	if (scale >= 1.0f) host_gui_sdl3_scale = scale;
#endif
	int w = 0, h = 0;
	SDL_GetWindowSize(window, &w, &h);
	if (w > 0 && h > 0)
	{
		rect->w = (uint64_t)(w / host_gui_sdl3_scale);
		rect->h = (uint64_t)(h / host_gui_sdl3_scale);
	}
}
#endif
```

### `src/host/gui_sdl3_event.h`

There is a new scale, `host_gui_sdl3_scale`. It is 1 everywhere except
Android, where SDL gives the window in pixels rather than points and the GUI
would otherwise come out half size or smaller. Every coordinate SDL reports is
divided by it: window resize, finger, pen, mouse motion and mouse button.

```cpp
static float host_gui_sdl3_scale = 1.0f;
...
out->x = (int32_t)(e.motion.x / host_gui_sdl3_scale);
out->y = (int32_t)(e.motion.y / host_gui_sdl3_scale);
```

The back buffer is already drawn stretched over the whole window, so nothing
else is needed to fill the screen.

### Not changed, but built differently

* `src/host/main.cpp` is compiled with `-Dmain=host_main`. The app's own main,
  `mobile/src/mobile_main.cpp`, calls it with
  `obj/vp64/VP64/sys/boot_image -e -run (... (node-net :full 1) (import {service/gui/app.lisp}))`.
* `src/host/pii_linux.cpp` on Android is compiled with
  `-include mobile/src/android_shm.h`, as bionic has no `shm_open`.

## Before the first build

```sh
make                                              # host programs
./run_tui.sh -n 0 -f -s mobile/make_vp64.lisp     # full VP64 object tree
```

Run the second again after changing any `.vp` file. Both build scripts fetch
SDL3 into `mobile/deps/SDL` and rebuild the tar of the system's files each
time.

## iOS Simulator

```sh
xcrun simctl list devices available | grep iPad   # pick one, note its id
xcrun simctl boot <device-id>
open -a Simulator
mobile/ios/build.sh -r                            # build, install, launch
```

A mouse click is a finger, and the Mac keyboard types into the app. To debug,
`open mobile/build/ios/ChrysaLisp.xcodeproj`, pick the `main` scheme and a
simulator, then Run.

A real device: `mobile/ios/build.sh -d TEAM_ID`, then install from Xcode.

## Android emulator

Make an AVD once. The Android Studio Device Manager works, or:

```sh
export JAVA_HOME="/Applications/Android Studio.app/Contents/jbr/Contents/Home"
SDK=~/Library/Android/sdk
$SDK/cmdline-tools/latest/bin/sdkmanager "system-images;android-36;google_apis;arm64-v8a"
echo no | $SDK/cmdline-tools/latest/bin/avdmanager create avd -n ChrysaLisp_Test \
	-k "system-images;android-36;google_apis;arm64-v8a"
```

Then start it, and build, install and launch:

```sh
~/Library/Android/sdk/emulator/emulator -avd ChrysaLisp_Test &
mobile/android/build.sh -r
```

The APK is `mobile/android/app/build/outputs/apk/release/app-release.apk`.
`build.sh -r` installs it on whatever `adb devices` shows, so a phone with USB
debugging on works the same way.

## Using it

* The first launch unpacks the files, then the Login Manager appears.
* Three fingers down at once show or hide the on-screen keyboard.
* App log: iOS in Xcode's console or the terminal `build.sh -r` ran in;
  Android with `adb logcat | grep ChrysaLisp`.
