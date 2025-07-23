# Getting Started with ChrysaLisp

Welcome to ChrysaLisp! This guide will walk you through setting up ChrysaLisp 
on your system, from installing dependencies to running your first applications 
and exploring its distributed networking capabilities.

ChrysaLisp is a unique Lisp environment designed for performance and systems-
level programming. It features a Virtual Processor (VP) as its compilation 
target, enabling portability across different hardware architectures.

## System Requirements and Dependencies

ChrysaLisp requires a 64-bit operating system. Below are the specific 
dependencies for macOS, Linux (including Raspberry Pi), and Windows.

### macOS

1. **Homebrew**: If you don't have Homebrew, install it from [brew.sh](https://brew.sh). 
   It simplifies installing other necessary software.

2. **Xcode Command Line Tools**: If not already installed, Homebrew might prompt 
   you, or you can install them manually:

```code
xcode-select --install
```

3. **SDL2 and SDL2_mixer**: These libraries are needed for the graphical user 
   interface (GUI) and audio. Install them via Homebrew:

```code
brew install sdl2 sdl2_mixer
```

4. **Git**: For cloning the repository (if you choose that method).

```code
brew install git
```

### Linux (Debian/Ubuntu based, including Raspberry Pi 64-bit)

1. **Build Essentials and Git**:

```code
sudo apt-get update
sudo apt-get install build-essential git
```

2. **SDL2 and SDL2_mixer**:

```code
sudo apt-get install libsdl2-dev libsdl2-mixer-dev
```

### Windows (64-bit)

1. **Build Environment**: A C++ compiler is needed if you plan to build the host 
   executables from source (e.g., MinGW-w64 or Visual Studio). However, the 
   release snapshot typically includes pre-built Windows executables 
   (`main_tui.exe`, `main_gui.exe`).

2. **SDL2 and SDL2_mixer**: Download the 64-bit development libraries:

    * SDL2: From [libsdl.org/download-2.0.php](https://www.libsdl.org/download-2.0.php) 
      (e.g., `SDL2-devel-2.x.x-mingw.tar.gz` or `SDL2-devel-2.x.x-VC.zip`).

    * SDL2_mixer: From the SDL_mixer page (linked from SDL's site) (e.g., 
      `SDL2_mixer-devel-2.x.x-mingw.tar.gz` or `SDL2_mixer-devel-2.x.x-VC.zip`).

    Extract the archives. You will need to copy the following DLL files from 
    their `lib/x64/` (or similar) directories into your main ChrysaLisp 
    directory later:

    * `SDL2.dll`

    * `SDL2_mixer.dll`

    * `libFLAC-8.dll` (or similar, if it's a dependency of SDL2_mixer you chose)

    * `libmodplug-1.dll` (or similar)

    * `libmpg123-0.dll` (or similar)

    * `libogg-0.dll` (or similar)

    * `libopus-0.dll` (or similar)

    * `libvorbis-0.dll` (or similar)

    * `libvorbisfile-3.dll` (or similar)

    (The exact list of SDL2_mixer dependencies might vary; ensure all required 
    DLLs are present).

3. **Git (Optional)**: If you plan to clone the repository, install Git for 
   Windows from [git-scm.com](https://git-scm.com).

### Raspberry Pi Framebuffer Mode (Advanced)

ChrysaLisp supports running directly on the Raspberry Pi's framebuffer, 
bypassing the X11 desktop environment. This is detailed in `framebuffer.md`. 
Here's a summary of the setup:

1. **Install Raspberry Pi OS (64-bit)**:

    * Use the Raspberry Pi Imager (from
    [raspberrypi.com/software](https://www.raspberrypi.com/software/)).

    * Choose "Raspberry Pi OS (other)" then "Raspberry Pi OS (64-bit)" (Lite or
    Desktop version).

    * Configure settings (hostname, SSH, user, Wi-Fi) before writing the image.

2. **Initial Pi Setup**:

    * Boot the Pi. If it's a desktop version, configure it to boot to CLI: Pi 
      logo (top left) -> Preferences -> Raspberry Pi Configuration -> System -> 
      "Boot: To CLI".

    * You can also use `sudo raspi-config` for various system settings.

3. **Configure Framebuffer for 32bpp ARGB**:

    * Edit `/boot/firmware/config.txt`:

```code
sudo nano /boot/firmware/config.txt
```

    * Comment out the `dtoverlay=vc4-fkms-v3d` line by adding `#` at the 
      beginning:

```code
#dtoverlay=vc4-fkms-v3d
```

    * Reboot. You can check the format with `fbset`.

```code
fbset

mode "1920x1080
    geometry 1920 1080 1920 1080 32
    timings 0 0 0 0 0 0 0
    rgba 8/16,8/8,8/0,8/24
endmode
```

4. **Set Permissions**: Replace `your_username` with your actual username.

```code
sudo usermod -a -G video your_username
sudo usermod -a -G input your_username
sudo usermod -a -G tty your_username
sudo chmod 666 /dev/tty
```

5. **Disable GPM (Console Mouse)**:

```code
sudo systemctl stop gpm
sudo systemctl disable gpm
```

6. **Reboot**:

```code
sudo reboot
```

## Downloading ChrysaLisp

You have two main options for getting the ChrysaLisp files:

* **Latest Release (Recommended for new users)**:

    1. Go to the [ChrysaLisp GitHub Releases page](https://github.com/vygr/ChrysaLisp/releases).

    2. Download the `ChrysaLisp-snapshot.zip` (or similarly named) file for the 
       latest release.

    3. Extract the ZIP archive to a directory of your choice. This archive 
       contains the source code along with essential pre-built components like 
       the VP64 boot image needed for bootstrapping the system.

* **Master Branch (Latest development version)**:

    1. Ensure Git is installed (see dependency sections above).

    2. Clone the repository:

```code
git clone https://github.com/vygr/ChrysaLisp.git
```

    3. This will create a `ChrysaLisp` directory with the latest source code.

## Installation and First Launch

The installation process involves two main stages:

1. Building the host C/C++ executables (`main_tui`, `main_gui`).

2. Using these host executables with a pre-supplied VP64 boot image to run a Lisp 
   script (`apps/tui/install.lisp`) that compiles the entire ChrysaLisp system 
   and creates the native boot image.

Navigate to your ChrysaLisp directory before running these commands.

### macOS / Linux / Raspberry Pi (Desktop/X11 mode)

1. **Build and Install**:

    This command compiles the C/C++ host programs and then uses the TUI host in 
    emulation mode (`-e`) with the pre-supplied VP64 boot image to run the 
    ChrysaLisp installation script. This script, in turn, compiles the ChrysaLisp 
    system, including generating the native boot image.

```code
make install
```

    You should see output indicating compilation progress.

2. **Run Text User Interface (TUI)**:

    This starts a network of ChrysaLisp nodes (default 10) and launches the 
    Terminal application.

```code
./run_tui.sh
```

    You should see the "ChrysaLisp Terminal" prompt.

3. **Run Graphical User Interface (GUI)**:

    This starts a network of ChrysaLisp nodes and launches the GUI environment, 
    typically starting with the Login Manager or Wallpaper and Launcher.

```code
./run.sh
```

### Windows (64-bit)

1. **Prepare DLLs**: If you haven't already, copy the required SDL2 and SDL2_mixer 
   DLLs (e.g., `SDL2.dll`, `SDL2_mixer.dll`, and their dependencies) into the root 
   of your ChrysaLisp directory where `main_tui.exe` and `main_gui.exe` will reside 
   or be built. The release snapshot usually includes pre-built `main_tui.exe` and 
   `main_gui.exe`.

2. **Build and Install (if pre-built executables are not used or need rebuilding)**:

    If you have a C++ build environment (like MinGW-w64 or Visual Studio's cl.exe 
    in your PATH) and want to rebuild the host executables, you can try running 
    `make install` (e.g., using `mingw32-make`). Otherwise, the pre-built 
    executables from the snapshot can be used with the `install.bat` script.

3. **Run Installation Script**:

    This uses the pre-built `main_tui.exe` in emulation mode with the VP64 boot 
    image from the snapshot to compile the ChrysaLisp system and generate the 
    native boot image.

```code
install.bat
```

4. **Run Text User Interface (TUI)**:

    * Using Batch:

```code
run_tui.bat
```

    * Using PowerShell:

        (First time, run PowerShell as Administrator and execute: 
        `Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope CurrentUser`)

```code
.\run_tui.ps1
```

5. **Run Graphical User Interface (GUI)**:

    * Using Batch:

```code
run.bat
```

    * Using PowerShell:

```code
.\run.ps1
```

### Raspberry Pi (Framebuffer Mode)

1. **Build and Install for Framebuffer**:

    From the text console on your Raspberry Pi (after completing the framebuffer 
    setup steps):

```code
make GUI=fb install
```

    This specifically builds the `main_gui` executable to use the Linux 
    framebuffer instead of X11/SDL2. The rest of the installation process is 
    similar to the standard Linux install.

2. **Run GUI (Framebuffer Mode)**:

    From the text console:

```code
./run.sh -f
```

    The `-f` flag tells the `run.sh` script to launch the GUI in framebuffer mode. 
    You should see the ChrysaLisp GUI directly on the console.

    To exit, use the Logout application (usually accessible from a main menu or 
    launcher) and select its "Quit" option. If `DEBUG` is enabled in the 
    framebuffer driver source (as it might be by default), pressing `ESC` will 
    also exit, but **this is a hard exit and will not save any state**.

### Stopping the ChrysaLisp Network

To shut down all background ChrysaLisp processes:

* **macOS / Linux / Raspberry Pi**:

```code
./stop.sh
```

* **Windows**:

    * Using Batch:

```code
stop.bat
```

    * Using PowerShell:

```code
.\stop.ps1
```

## Exploring Network Topologies (Advanced)

ChrysaLisp is designed for distributed computing. The standard `run.sh` and 
`run_tui.sh` (and their `.bat`/`.ps1` counterparts) start a default network 
topology (often a fully connected mesh of 10 nodes). You can explore different 
network setups using specialized scripts. These scripts typically accept common 
arguments:

* `-n <count>`: Specifies the number of VP nodes to launch.

* `-b <base_id>`: Sets the starting node ID for this group of nodes.

* `-e`: Runs the nodes using the VP64 emulator with the VP64 boot image (slower, 
  but useful for debugging or if native compilation is an issue).

* `-f`: (For GUI scripts like `run.sh`) Runs the primary node in the foreground; 
  closing it will trigger a shutdown of other nodes. For TUI scripts, it often 
  means the primary TUI is attached to the first node.

Common topology scripts (found in the root ChrysaLisp directory):

* `run_ring.sh/.bat/.ps1`: Connects nodes in a ring topology. Each node is 
  connected to two neighbors.

* `run_mesh.sh/.bat/.ps1`: Connects nodes in a 2D mesh (grid) topology. Each node 
  is connected to up to four neighbors (up, down, left, right, with wraparound).

* `run_cube.sh/.bat/.ps1`: Connects nodes in a 3D cube (toroidal mesh) topology. 
  Each node is connected to up to six neighbors.

* `run_tree.sh/.bat/.ps1`: Connects nodes in a binary tree topology.

* `run_star.sh/.bat/.ps1`: Connects all peripheral nodes to a central hub node.

**Example**: To run an 8-node ring network on macOS/Linux:

```code
./run_ring.sh -n 8
```

The first node (node 0 by default if `-b` is not used) will typically start the 
GUI or TUI.

## Extending with ChrysaLib (Optional)

ChrysaLib is a C++17 library that implements the ChrysaLisp VP message protocol
and allows for communication with ChrysaLisp VP nodes. This enables more
advanced scenarios:

* **Bridging Lisp Subnets**: You can connect ChrysaLisp networks running on 
  different machines using ChrysaLib's `hub_node` acting as a bridge. This can 
  be over USB, Thunderbolt (treated as fast IP), or standard IP networks.

* **Heterogeneous Networks**: Create a larger, distributed ChrysaLisp network 
  spanning multiple physical machines, potentially with different architectures 
  if ChrysaLisp supports them.

* **C/C++ Services**: Develop services in C++ that can run as part of the 
  ChrysaLisp VP network, interacting with Lisp tasks via message passing.

To use ChrysaLib:

1. Clone and build the ChrysaLib project from its GitHub repository (details will 
   be in its own `README.md`).

2. The `link` command within the ChrysaLisp Terminal application is used to 
   establish connections to ChrysaLib hubs. For example, to connect two machines 
   (Machine A and Machine B at IP 192.168.1.100):

    On Machine A (acting as a local hub):

```code
../../ChrysaLib/hub_node -shm 192.168.1.100 &  # Or your path to hub_node
./run.sh                                      # Or run_tui.sh
# In ChrysaLisp terminal:
link CLB-L1
```

    On Machine B (connecting to Machine A's hub):

```code
../../ChrysaLib/hub_node -shm &                # Or your path to hub_node
./run.sh                                      # Or run_tui.sh
# In ChrysaLisp terminal:
link CLB-L1
```

    The `CLB-L1` is a symbolic name for the shared memory link. Both ChrysaLisp 
    instances will then see each other's nodes and services.

## What's Next?

You now have a running ChrysaLisp environment! Here are some suggestions for 
further exploration:

* **Documentation**: Browse the `docs/` directory in your ChrysaLisp installation. 
  It contains detailed information on various aspects of the system.

* **Reference Manuals**: You can generate up-to-date reference documentation by 
  running the `make docs` command from within the ChrysaLisp TUI (Terminal 
  application). This will scan the source code and create Markdown files in 
  `docs/reference/`.

* **Example Applications**: Explore the applications in the `apps/` directory. 
  They showcase GUI development, networking, and various Lisp features.

* **Command-Line Tools**: Try out the different command-line tools available in 
  the `cmd/` directory from the ChrysaLisp Terminal.

* **Lisp REPL**: Launch the Lisp Read-Eval-Print Loop by typing `lisp` in the 
  ChrysaLisp Terminal to experiment with the language directly.
