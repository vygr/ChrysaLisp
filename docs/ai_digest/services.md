# Services: Names, How Many, and One of a Kind

A service is a task with a mailbox it gives a name to. Other tasks find it
by that name and mail it. This is how they are named, how far a name
reaches, how many of each there are meant to be, and how a service that is
to be one stays one when it is started twice at once.

## A name and how far it reaches

`(mail-declare mbox name info)` says the name, and gives back the entry to
forget it by, `(mail-forget entry)`. `(mail-enquire prefix)` lists the
entries there are, each `name,mailbox,system,info`. The first letter of the
name says how far it goes:

| Name | Seen by | Carried by |
|---|---|---|
| `@Lock` | the nodes of its own system, its machine | the pings a node sends its own machine's nodes |
| `*Mesh` | every node of the mesh | the pings a node sends every node |
| `Todo` | its own node only, with neither letter | nothing, it is not sent |

A name comes to a node by its pings, so a node that has just started a
service is not seen to have it everywhere at once, but a moment later,
[messages_and_routing.md](messages_and_routing.md).

To find one, `sys/lisp.inc` has:

* `(mail-first name)`, the first the list has.
* `(mail-here name)`, one on this node, whose task is still there: the
  desktop's own.
* `(mail-service name)`, the one on this node if it is there, else any.
* `(mail-lowest name)`, the one with the lowest mailbox, of all those seen.
* `(mail-single name mbox)`, `:nil` if the service of that mailbox is the one
  that stays, else the one it gives way to.

## How many of each

| Service | How many | Started |
|---|---|---|
| `@Lock`, `@Net` | one to a network: one machine may run several, a desktop and a rack member say, each with its own | if none is listed |
| `@Source` | one to a system; two networks of one machine that are linked share one | if none is listed, giving way, below |
| `@Audio`, `@Voice`, `@Clipboard` | one to a desktop: each desktop has its own screen, speaker and microphone | if this node has none, `(mail-here)` |
| `@Gui` | one to a desktop: two desktops at once on a machine are allowed, for two screens, `run.sh -g`. Each also says `Gui`, which only its own node sees, so a task there finds its own desktop's | if this node has none |
| `*Mesh` | one to a machine's Net service | by the Net service |
| `*Chat`, `*Call`, `*Whiteboard` | one to each that offers itself | by the app |

## One of a kind

A service that is to be one is started by whoever needs it and finds none
listed. Two may look at the same moment, both find none, and both start
one: the name of the first had not yet come to the second. So a second is
not prevented; it is made to give way, safely, by a rule they all know.

```image
docs/diagrams/service_give_way.cwb
```

* **The rule.** Of the copies listed, the one with the lowest mailbox stays.
  Each works that out from the list alone, `(mail-lowest)`, so all agree
  with no word between them.
* **The check.** A copy asks `(mail-single name its_mailbox)` each time it
  wakes: when a task asks it something, and once a second. Till the other's
  name has come to it the answer is `:nil`, and it serves as normal.
* **Giving way.** A copy told to give way forgets its name at once, so the
  list shows one from then on, and every task that looks finds the one that
  stays. A request that still comes to it, one sent before the sender saw
  both, is sent on to the one that stays, and the reply goes straight back
  to the sender: each request is answered once. It answers the tasks it was
  already working for, and goes when nothing has come to it for as long as
  a task waits for an answer, 10 seconds.
* **Who finds it.** A task looks for the service by `(mail-lowest)`, so as
  soon as it sees both it uses the one that stays.

`@Source` does this, `service/source/app_impl.lisp`, and
`tests/system/test_source.lisp` starts two copies at once and counts that
every copy answers and one stays. It suits a service that any copy may
answer for: what `@Source` holds is a cache, and two of them for a moment
cost only a file asked for twice.

It does not suit one that holds state. `@Lock` grants locks: a copy that had
granted one before it knew it was to give way, then sent the next request
on, would leave two locks on one thing. `@Net` makes the machine's links. For
those, and for `*` services that hold state, a second must never act at
all, which giving way does not give; that is still to be designed. They
stay as they are: one to a network, started if none is listed.
