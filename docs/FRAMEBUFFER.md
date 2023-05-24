# Introduction to Framebuffer Operations

## Getting Started on Raspberry Pi 3

ChrysaLisp now runs on 64-bit ARM-based Raspberry Pi systems.
In order to get started, if you're not already running
on their 64-bit Linux (known as Bullseye), you'll first need to download
the Raspberry Pi system imager from:
https://www.raspberrypi.com/software/

Running the imager will let you select an operating system, 
storage device and target microSD storage card to write to.
To install the required 64-bit Linux, hit "Choose OS", then
"Raspbery Pi OS (other", then
"Raspberry Pi OS (64-bit)". Click on the Settings icon in the
lower right and select "pi.local" for the hostname, Enable SSH,
login username/password and your wireless LAN SSID and password.

This will write the target microSD storage card with the specified
settings which can then be used to boot 64-bit Bullseye Linux
on the PI.

NOTE: ChyrsaLisp *requires* a 64-bit OS, and currently won't run on 
32-bit Linux.

### Setting up the Pi system after bootup

After the first boot, the Pi will come up running a
graphical X11 desktop. You'll want to turn this off
by clicking on the Pi logo in the top left, then
selecting Preferences->Raspberry Pi Configuration.
Then select System->"Boot: to CLI", OK. You might
want to change your display to 1920x10280,
this can be done by Selecting Display->Headless Resolution.

After rebooting using Pi->Shutdown->Reboot,
the system should come up in a text-mode console.

When running text-mode, many settings can be changed by running the system-wide
setup tool on Pi, including the ability to switch back to graphical
desktop mode on boot, if wanted:

```code
sudo raspi-config
```

The next step is setting up the framebuffer device to use 32bpp
(bits per pixel) ARGB. This will result in higher quality graphics.
To do this, edit the /boot/config.txt file using:

```code
sudo vi /boot/config.txt
```

Find the following line, and comment out the statement
by adding a '#' as the first character. This removes the
default framebuffer driver, which defaults to RGB565.
(Note, ChyrsaLisp will run fine on RGB565 if you want to):
```code
#dtoverlay=vc4-fkms-v3d     <--- add '# at line start for 32bpp
```
Type ":wq" to save.

The program `fbset` can be used (after rebooting) to see
what the current framebuffer format is.

By default, Linux doesn't allow opening /dev/fb0 (the framebuffer),
nor the default terminal (/dev/tty), or the mouse (/dev/input/mice)
unless the device file is allowed to with group or other permissions.
To change that, run the following, presuming you're logged in as 'user1':

```code
usermod -a -G video user1
usermod -a -G input user1
sudo chmod 666 /dev/tty
```

If you're running the GPM mouse, it should be turned off using:

```code
sudo systemctl stop gpm
sudo systemctl disable gpm

```

Then reboot using `reboot`. The system should come up in text mode with everything
ready to go for running ChrysaLisp.

### Building ChyrsaLisp on PI

To pull down the source and build ChyrsaLisp, use the following:

```code
git clone https://github.com/vygr/ChrysaLips.git
make GUI=fb install
sudo apt install vim    (not needed, but nice to have full vi editor)
```

### Running ChrysaLisp on framebuffer after setup

To run ChyrsaLisp from the text console, use:
```code
./run.sh -n 1
```
If all the permissions are right, you should see a graphical screen
and can Login. If not, their should be an error message showing
the problem, which is likely a permissions issue described above.

To exit (for now), type ESC. Be aware that ESC
will immediately exit ChrysaLisp without saving anything. We hope
to have an update that allows exiting from the GUI itself shortly.

To stop the system from outside of ChrysaLisp, use:

```code
./stop.sh
```
