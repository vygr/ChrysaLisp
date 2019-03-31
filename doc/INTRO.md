# Intro

## Getting Started

First of all you need to download the zip from Github or clone the Github repo
using your `git clone` command.

```
git clone https://github.com/vygr/ChrysaLisp.git
```

This will create a ChrysaLisp directory with all you need to try the demos and
develop for ChrysaLisp.

### Mac

Download and install the SDL frameworks from:
https://www.libsdl.org/download-2.0.php and
https://www.libsdl.org/projects/SDL_ttf/

Get the development *.dmg* files for both frameworks and copy *SDL2.framework*
and *SDL2_ttf.framework* over into your */Library/Frameworks* folder.

### Linux, PI64

You should be able to use your package manager to install via `apt-get` or
similar with:

```
sudo apt-get install libsdl2-ttf-dev
```

### Windows 64bit

Unzip the snapshot.zip file with your favourite Zip application. With 7Zip just
right click on the snapshot.zip file and choose 'Extract Here'.

Download and install the SDL frameworks from:
https://www.libsdl.org/download-2.0.php and
https://www.libsdl.org/projects/SDL_ttf/

Get the development versions for both frameworks and unzip them somwhere. Copy
the .dll files from the lib folders into your ChrysaLisp folder.

SDL2.dll
SDL2_ttf.dll
libfreetype-6.dll
zlib1.dll

## Building the platform bootstrap

### Mac, Linux, PI64

Go into the ChrysaLisp directory and type `make`. This will unpack the boot
image files, create the ChrysaLisp directory structures and use your C compiler
to compile and link the platform bootstrap executable.

There are a few other options available `clean` `boot` that you might need as
you get further involved, details are in the README.md file.

### Windows 64bit

Currently the main.exe bootstrap comes prebuilt. You can by all means create a
Visual Studio project and compile the main.c bootstrap yourself. Enjoy.

## Running

### Mac, Linux, PI64

After a successful `make` of the platform bootstrap you can now run the TUI
(Text User Interface) or the GUI (Graphical User Interface) and try the demos.

For a TUI or GUI type:

```
./run_tui.sh
./run.sh
```

If you wish to try a more complex network demo, there are ring, mesh, cube,
tree and star network launch files provided. Documentation for these is
provided in the README.md file.

### Windows 64bit

```
.\run_tui.bat
.\run.bat
```

## Stopping

In the TUI situation you can always get back to your platform terminal prompt
by `CNTRL-C` in the terminal window.

If you wish to shutdown all the background processes that are created to
simulate the network of virtual CPU's, use:

### Mac, Linux, PI64

```
./stop.sh
```

### Windows 64bit

```
.\stop.bat
```

## Building ChrysaLisp

From the TUI or from the GUI Terminal app, the `make` command allows you to
compile the system, create a new boot image, cross compile for the various
supported platforms, create their boot images and scan the source files to
build the reference documentation.

```
make doc
```

Scan source files and create the reference documents.

```
make
```

Compile any files required based on the age of edited source files and produced
binaries.

```
make all
```

Compile all source files, regardless of ages of files.

```
make boot
```

Create a boot_image file from the function binaries within the *obj/*
directory.

```
make platforms
```

Compile and build all platforms not just the current platform.

You can use combinations if you like. Such as:

```
make all platforms boot
```

This will compile all files for all platforms and create the boot_image files
for all platforms.

## Running the Lisp

At a ChrysaLisp terminal prompt type:

```
lisp
```

You will see the sign on message for the Lisp interpreter and then you can use
the REPL directly to experiment.
