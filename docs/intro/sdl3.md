# Moving From SDL2 To SDL3

The GUI used SDL2, and SDL2_mixer for sound. It now uses SDL3, and nothing
else. This is what changed, what to install, and what to do on a machine that
SDL3 does not come ready made for.

## What Changed

* `make install`, and `make`, build the SDL3 GUI driver if SDL3 is on the
  machine, and the SDL2 driver if it is not. It says which as it starts,
  `Building sdl3 GUI driver.` To pick, `make GUI=sdl3 install` or
  `make GUI=sdl install`.

* There is no mixer library. The SDL3 audio driver does its own mixing.

* On the SDL3 driver a shader can be drawn by the GPU, the GPU button of the
  Surface demo, see `docs/ai_digest/shader_language.md`. That needs SDL 3.4 or
  later. On SDL 3.2 the GUI is the same and the button says there is no GPU.

* The SDL2 driver is still there and still works. It can not draw a shader.

## What Each Machine Needs

| Machine | What to do | Shaders on the GPU |
| :--- | :--- | :--- |
| A recent Mac | `brew install sdl3` | yes |
| A Mac too old for Homebrew to have SDL3 | build SDL3 from source | yes |
| Debian 13, Raspberry Pi OS on it | `apt-get install libsdl3-dev`, it is 3.2 | no |
| The same, for shaders | build SDL3 from source | yes |
| An older Debian or Raspberry Pi OS | stay on SDL2, or build SDL3 from source | SDL2 no |
| Windows | `install.bat` fetches `SDL3.dll` | yes |

## SDL3 From Source

It is one library with no others to find, and it builds with `cmake`. This
puts it in a folder of its own, in your home folder, and touches nothing
else on the machine.

```code
mkdir ~/sdl3_build
cd ~/sdl3_build
git clone --depth 1 -b release-3.4.16 https://github.com/libsdl-org/SDL.git
cmake -S SDL -B build -DCMAKE_BUILD_TYPE=Release \
	-DCMAKE_INSTALL_PREFIX=$HOME/sdl3_build/install \
	-DSDL_TESTS=OFF -DSDL_EXAMPLES=OFF
cmake --build build -j 4
cmake --install build
```

Then tell the ChrysaLisp build where it is, once. It is kept in a file,
`sdl3_prefix`, so later builds need not be told.

```code
cd ~/ChrysaLisp
make install SDL3_PREFIX=$HOME/sdl3_build/install
```

On a Raspberry Pi 4 the build of SDL3 takes a little over three minutes.

### On A Mac

`cmake` is needed. If Homebrew will not give you one, `pip3 install cmake` in
a Python virtual environment does. Nothing else is needed, the Xcode command
line tools have the rest.

### On Debian, And A Raspberry Pi

SDL3 finds what the machine has when it is configured, so the development
packages for the display, the sound and the input have to be there first, or
it builds without them.

```code
sudo apt-get install build-essential git cmake pkg-config \
	libdrm-dev libgbm-dev libegl-dev libgles-dev libvulkan-dev \
	libasound2-dev libpipewire-0.3-dev libpulse-dev \
	libudev-dev libdbus-1-dev libxkbcommon-dev \
	libwayland-dev wayland-protocols libdecor-0-dev \
	libx11-dev libxext-dev libxrandr-dev libxcursor-dev \
	libxfixes-dev libxi-dev libxss-dev libxtst-dev
sudo apt-get install mesa-vulkan-drivers
```

## A Raspberry Pi With No Desktop

The SDL3 driver runs on the bare display, Raspberry Pi OS Lite with a screen
on the HDMI port and nothing else, no X11 and no Wayland. `./run.sh` from a
login on the Pi, or over ssh, takes the whole screen. This was run on a Pi 4
with 2GB, on Debian 13, with SDL 3.4.16 from source, and the Pi's own GPU
draws the shaders, through Vulkan.

Two things to know. Plug the screen in before the Pi is started, plugged in
later the picture came and went the first time the GUI ran. And the wireless
USB mouse we tried on that Pi lagged badly, a Bluetooth mouse did not.

Sound goes to the Pi's default sound device, which is the headphone socket,
not the TV. To send it down the HDMI cable, make the first HDMI port the
default, and start ChrysaLisp again.

```code
printf "defaults.pcm.card 1\ndefaults.ctl.card 1\n" | sudo tee /etc/asound.conf
```

`aplay -l` lists the cards, `vc4hdmi0` is the port next to the power socket.

The frame buffer driver, `make GUI=fb install`, is still there for a Pi with
no SDL at all, see `docs/intro/framebuffer.md`.

## Windows

`install.bat` fetches `SDL3.dll` if it is not in the ChrysaLisp folder, and
the programs in the snapshot are built for it. The SDL2 DLLs, `SDL2.dll`,
`SDL2_mixer.dll` and the rest, are not used any more and can be deleted.

The Windows programs are built on a Mac. Martyn Blyss ran them on Windows,
the install and its fetch of `SDL3.dll`, the GUI from `run.bat` and from
`run.ps1`, and the Surface demo on the GPU. For shaders the driver asks SDL
for Vulkan, which needs a graphics driver that has it, most do.

To build them, `make -f Makefile.mingw windows_all`, and for SDL2,
`make -f Makefile.mingw GUI=sdl windows_all`.

## Staying On SDL2

`make GUI=sdl install` on any machine. With no SDL3 on the machine that is
what `make install` does anyway. Nothing is lost but the GPU shaders.

If SDL3 is installed later, the next `make` picks it up and builds the SDL3
driver. To hold a machine on SDL2 give `GUI=sdl` each time.
