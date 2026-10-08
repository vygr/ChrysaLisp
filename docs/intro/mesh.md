# Joining Machines Together

Two or more machines running ChrysaLisp on the same network can join into
one. Every node of every machine sees all the others, a build spreads over
all their cores, and the Network Map shows the lot. This is how to do it.

It is three steps, and the first you do once.

## 1. Make a key, once

A key is a small file that says which machines belong together. Only
machines with the same key will join each other.

On one machine, start ChrysaLisp, open a Terminal, and type:

```code
mesh -k
```

That makes a file called `mesh_key` in the ChrysaLisp folder, next to
`run.sh`.

Copy that file into the ChrysaLisp folder of each other machine, in the
same place. Use a USB stick, AirDrop, a shared folder, whatever you have.
ChrysaLisp never sends it for you, and git does not track it.

If ChrysaLisp was already running on a machine when you copied the key
there, stop it and start it again. The key is read at the start.

You can skip the key. Then any machine on your network that runs
ChrysaLisp and joins will be let in. On a network you share, make a key.

## 2. Have every machine on the same version

Update each machine and build it, the usual way:

```code
git pull
make install
```

Machines on different versions may link and then not see each other.

## 3. Join, on each machine

On every machine, start ChrysaLisp, open a Terminal, and type:

```code
mesh -j
```

It listens for the others, tells the network it is there, and looks for
them. After a few seconds, up to half a minute, it lists who it has found:

```code
Key:     mesh_key, only machines with the same file can join.
Joined:  yes.
Machines:
    8921786E    7 nodes  arm64/Darwin  (this machine)
    D649CA0E    4 nodes  arm64/Linux
    C96A1B10    8 nodes  x86_64/Darwin
```

That is it. You join again each time you start ChrysaLisp, it does not
join by itself.

## See it

Open **Network Map**, under System in the launcher. Each node is a ball,
the color of its machine. Each link is a bar, thicker between machines,
and it glows from blue through red to white as mail flows over it. Start a
machine, or stop one, and watch it arrive or go.

`mesh`, with nothing after it, says how things stand at any time. `nodes`
lists every node there is.

## If a machine is not found

`mesh` on each machine tells you most of it. Go down this list:

* **Has it joined?** `mesh -j` on that machine too. Every machine has to.

* **The key.** Both have the file `mesh_key`, the same file, in the
  ChrysaLisp folder, or neither has. And ChrysaLisp was started after the
  file was put there.

* **The version.** `git pull` and `make install` on both.

* **The firewall.** Each machine must let in TCP port 3333 and UDP port
  3334. macOS asks the first time, "Do you want the application main_gui
  to accept incoming network connections?", say Allow. Windows asks the
  same of Windows Defender Firewall, allow it on private networks.

* **The network.** Machines find each other by calling out on the local
  network. That does not cross from one network to another, and some
  Wi-Fi, a guest network say, stops machines talking to each other at
  all. If they are not found, name one machine by its address:

```code
mesh -t 192.168.1.100
```

  It joins by way of that machine, which tells it who the others are. The
  machine named must have joined already.

## What joining lets in

A machine that has joined runs tasks for the others, that is the point of
it. With a key, only the machines you gave the key to. It does not let
them write to its files, that is a separate thing you would have to turn
on, `sync -a`, and is not needed to join.

## Under it

`mesh -j` is two commands, `link -l 3333 -a` and `link -a`, and `mesh -k`
is `link -k`. The `link` command has more, a port of your choice, a link
to one named machine and no more. `link -h` lists it, and
`docs/intro/intro.md` has the detail.

Windows has all of this in it and has been tried the least. If it does not
work there, the list above first, then tell us.
