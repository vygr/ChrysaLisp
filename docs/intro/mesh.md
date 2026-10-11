# Joining Machines Together

Two or more machines running ChrysaLisp on the same network can join into
one. Every node of every machine sees all the others, a build spreads over
all their cores, and the Network Map shows the lot. This is how to do it.

It is one thing to do: give them the same key. The rest they do by
themselves.

## 1. Give them the same key

A key is a small file that says which machines belong together. Only
machines with the same key will join each other.

Every machine has one from the day it is installed, `make install` makes
it: the file `mesh_key` in the ChrysaLisp folder, next to `run.sh`. Each
machine's is its own, so a new machine is by itself, and nothing on your
network can join it.

Pick one machine. Copy its `mesh_key` into the ChrysaLisp folder of each
other machine, in the same place, over the one that is there. Use a USB
stick, AirDrop, a shared folder, whatever you have. ChrysaLisp never
sends it for you, and git does not track it.

That is all. A machine that is running sees the file change, within a
few seconds: it ends the links it had, and joins the machines that have
the new key. One that is not running joins when it is next started.

Take the file away and the machine is off the network till it is put
back. There is no joining without a key.

A tree that was never installed, or was installed before this, may have
no key. `mesh -k` makes one.

## 2. Have every machine on the same version

Update each machine and build it, the usual way:

```code
git pull
make install
```

Machines on different versions may link and then not see each other. If
that happens `mesh` says so, it names the machine and says it is on a
different build.

## 3. See who is there

On any machine, open a Terminal, and type:

```code
mesh
```

A machine listens for the others, tells the network it is there, and
looks for them, from when it starts. After a few seconds, up to half a
minute, they have found each other:

```image
docs/diagrams/mesh_join.cwb
```

```code
Key:     mesh_key, only machines with the same file can join.
Joined:  yes.
Machines:
    8921786E    7 nodes  arm64/Darwin  (this machine)
    D649CA0E    4 nodes  arm64/Linux
    C96A1B10    8 nodes  x86_64/Darwin
```

## More than one ChrysaLisp on a machine

You can have a desktop and a terminal session up on one machine, or two
of either. One of them is the machine's member of the mesh: first come,
first served, the first to start. The others are by themselves, and
`mesh` in one of them says it has not joined. When the first is closed
another takes its place, within ten seconds or so.

## See it

Open **Network Map**, under System in the launcher. Each node is a ball,
the color of its machine. Each link is a bar, thicker between machines,
and it glows from blue through red to white as mail flows over it. Start a
machine, or stop one, and watch it arrive or go.

`mesh`, with nothing after it, says how things stand at any time. `nodes`
lists every node there is.

## If a machine is not found

`mesh` on each machine tells you most of it. Go down this list:

* **Has it joined?** `mesh` on that machine too. If it says it has not,
  and it has a key, another ChrysaLisp on that machine is its member.

* **The key.** Both have the file `mesh_key`, the same file, in the
  ChrysaLisp folder.

* **The version.** `git pull` and `make install` on both. `mesh` names a
  machine it can hear that is on a different build.

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

  It joins by way of that machine, which tells it who the others are.

## What joining lets in

A machine that has joined runs tasks for the others, that is the point of
it, and only for the machines you gave its key to. It does not let them
write to its files, that is a separate thing you would have to turn on,
`sync -a`, and is not needed to join.

## Under it

The Net service, which every session starts, does it: it watches the key
file, listens on TCP port 3333, calls out on UDP port 3334, and links to
each machine it hears that proves it has the key. `mesh -j` asks it to
again, the two commands `link -l 3333 -a` and `link -a`, and `mesh -k` is
`link -k`. The `link` command has more, a port of your choice, a link to
one named machine and no more. `link -h` lists it, and
[`docs/intro/intro.md`](intro.md) has the detail.

A subnet started as a system of its own, `nodes -s cube -o`, is not the
machine's: it has none of its key and is off the network, whatever the
machine it is on has joined.

Windows has all of this in it and has been tried the least. If it does not
work there, the list above first, then tell us.
