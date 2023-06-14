# Introduction to Framebuffer Operations

## Getting Started on Raspberry Pi3/4

ChrysaLisp now runs on 64-bit ARM-based Raspberry Pi systems. In order to get
started, if you're not already running on their 64-bit Linux (known as
Bullseye), you'll first need to download the Raspberry Pi system imager from:
`https://www.raspberrypi.com/software/`

Running the imager will let you select an operating system, storage device and
target microSD storage card to write to. To install the required 64-bit Linux,
hit "Choose OS", then "Raspberry Pi OS (other)", then "Raspberry Pi OS
(64-bit)" or "Raspberry Pi OS Lite (64-bit)" .

Click on the Settings icon in the lower right and select "pi.local" for the
hostname, Enable SSH, login username/password and your wireless LAN SSID and
password.

This will write the target microSD storage card with the specified settings
which can then be used to boot 64-bit Bullseye Linux on the PI.

NOTE: ChrysaLisp *requires* a 64-bit OS, and currently won't run on 32-bit
Linux.

### Setting up the Pi system after bootup

After the first boot, the Pi may come up running a graphical X11 desktop.
You'll want to turn this off by clicking on the Pi logo in the top left, then
selecting Preferences->Raspberry Pi Configuration. Then select System->"Boot:
to CLI", OK. You might want to change your display to 1920x1280, this can be
done by Selecting Display->Headless Resolution.

After rebooting using Pi->Shutdown->Reboot, the system should come up in a
text-mode console.

When running text-mode, many settings can be changed by running the system-wide
setup tool on Pi, including the ability to switch back to graphical desktop
mode on boot, if wanted:

```code
sudo raspi-config
```

The next step is setting up the FRAMEBUFFER device to use 32bpp (bits per
pixel) ARGB. This will result in higher quality graphics. To do this, edit the
`/boot/config.txt` file using:

```code
sudo nano /boot/config.txt
```

Find the following line, and comment out the statement by adding a '#' as the
first character. This removes the default FRAMEBUFFER driver, which defaults to
RGB565. (Note, ChrysaLisp will run fine on RGB565 if you want to):

```code
#dtoverlay=vc4-fkms-v3d  <--- add '# at line start for 32bpp
```

The program `fbset` can be used (after rebooting) to see what the current
FRAMEBUFFER format is.

By default, Linux doesn't allow opening `/dev/fb0` (the FRAMEBUFFER), nor the
default terminal `/dev/tty` (the CONSOLE), or the mouse `/dev/input/mice`
unless the device file is allowed to with group or other permissions. To change
that, run the following, presuming you're logged in as 'user1':

```code
sudo usermod -a -G video user1
sudo usermod -a -G input user1
sudo usermod -a -G tty user1
sudo chmod 666 /dev/tty
```

If you're running the GPM mouse, it should be turned off using:

```code
sudo systemctl stop gpm
sudo systemctl disable gpm
```

Then reboot using `reboot`. The system should come up in text mode with
everything ready to go for running ChrysaLisp.

### Building ChrysaLisp on PI

To pull down the source and build ChrysaLisp, use the following:

```code
sudo apt install git
git clone https://github.com/vygr/ChrysaLisp.git
cd ChrysaLisp/
make GUI=fb install
```

### Running ChrysaLisp on FRAMEBUFFER after setup

To run ChrysaLisp from the text console, use:

```code
./run.sh -f
```

If all the permissions are right, you should see a graphical screen and can
Login. If not, their should be an error message showing the problem, which is
likely a permissions issue described above.

To exit use the Logout application `Quit` option from the pop up dialog. If you
have DEBUG set in the FB driver code, type ESC. Be aware that the ESC key DEBUG
option will immediately exit ChrysaLisp without saving anything !

To stop the system from outside of ChrysaLisp, use:

```code
./stop.sh
```
