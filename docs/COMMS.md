# Communications

In ChrysaLisp while it is not forbidden for processes to communicate with
shared memory structures, provided they live on the same CPU node, the main
means that processes use to communicate with each other is via message passing.
Each process on creation is allocated a main mailbox, processes create messages
and send these messages between themselves by use of these mailboxes. More
mailboxes can be allocated, if required, to allow other communication
structures to be created.

All the mailbox and message header structures can be found in the
`sys/mail/class.inc` file.

## Mailboxes

Mailboxes are identified by an ID, this ID is allocated when the mailbox is
constructed using the `'sys_mail :alloc_mbox` function. The mailbox ID does not
directly address the mailbox structure that's used to hold received messages.

A Mailbox can be freed using the mailbox free function, `'sys_mail :free_mbox`.

A mailbox ID can be turned into the address of the message containing structure
using the `'sys_mail :validate` function. This mailbox address function
validates that this ID is an existing mailbox and will return `null` if the
mailbox cannot be found.

Mailbox IDs are always a unique 32 bit non repeated identifier. During mail
delivery any messages using an ID that cannot be validated are treated as junk
mail and sent to this nodes Postman task. This postman task just currently
frees the message but later versions of the system may use this information for
debugging purposes.

## Messages

Messages consist of two parts, the message header and the message body. The
message body consists of a referenceable object, most often a String object.
The header is like a postage stamp on the objects data. Large objects will have
multiple message headers created, by the system, to break up the sending of the
data into smaller packets. These packets are routed from the source node to the
destination node and reconstructed at the destination into the original message
before delivery into the target mailbox.

Individual message packets travel via parallel communication routes across the
network from source to destination. No guarantee is given for the order in
which message packets will arrive. Large messages that have been broken down
into smaller fragments are reconstructed piece by piece at the destination and
are only delivered into the recipient mailbox once all packets have been
received and reconstruction has finished. A receiver will never see fragments
of a large message, they receive it whole or not at all.

There are other mechanisms available, within the class library `class/in` and
`class/out`, to create and send a stream of messages while preserving an exact
sequence of delivery to the receiving process. They also hide the use of the
underlying messaging system within a streams based API.

There are two functions to allocate mail messages, one where the data is
already available `'sys_mail :alloc_obj`, and one where an empty message needs
to be created, which the creator will fill in with data, `'sys_mail :alloc`.

There are counterparts for the receiver where a message can be freed in its
entirety, `'sys_mail :free`, or the data object can be retained and only the
message header freed, `'sys_mail :free_obj`.

## Sending and Receiving

Once a mail message is allocated and ready for delivery the creating process
sends the message on its way with the `'sys_mail :send` function. This hands
the message over to the link processes and begins the routing of the message to
the destination mailbox.

A process receives messages by use of the `'sys_mail :read` function. This
function retrieves the next available mail message from the mailbox or blocks,
by suspending the process, until a message is received.

It is possible to examine a mailbox to see if any mail is available, without
suspending the calling process, by use of the `'sys_mail :poll` function.

Polling of multiple mailboxes and selection from multiple mailboxes is provided
by the `'sys_mail :poll` and `'sys_mail :select` function calls.

## Process Mailbox ID's

A `net_id` process mailbox ID consists of a combination of the local mailbox ID
and the CPU `node_id`. The CPU `node_id` occupies the later 128 bits, the local
mailbox ID occupies the first 64 bits.

Message routing first of all routes messages from the source CPU node to the
destination CPU node and then the local mailbox ID is validated before delivery
of the message into the receivers mailbox. Any message with an invalid local
mailbox ID is discarded as junk mail.

On creation of a process it is allocated a main local mailbox and the `net_id`
is returned to the creator. There are functions provided to create multiple
child processes in a single call. Such functions can be used to create farms,
arrays, and pipelines of processes. These functions return a list of `net_id`s.
The creator can then go on to wire these mailboxes and processes in any
communications structure it desires.

Auto allocated main process mailboxes do not need to be manually freed, this
will happen when the process shuts down.

## Services

Functions are also provided to allow mailboxes to be named, via `'sys_mail
:declare`, throughout the network. Such mailboxes are then discoverable by
other processes via a call to `'sys_mail :enquire` with the given name. A
service entry can be removed with the `'sys_mail :forget` function.

The system maintains a directory of these service names and the corresponding
process `net_id`. An example service is the current GUI `DEBUG_SERVICE`
application `apps/debug/app.lisp`.

### VP example

This is a network monitoring application child process. The child process
simply waits for a command message from the parent and either exits or returns
a message containing task and memory usage information.

`'sys_mail :mymail` is just a convenience function to read mail from the
current tasks main mailbox.

```vdu
(include "sys/func.inc")
(include "sys/kernel/class.inc")

(def-struct reply 0
	(struct node_id node_id_size)
	(uint task_count mem_used))

(def-func 'apps/netmon/child)
	;monitor task

	(def-vars
		(ptr msg data reply rdata))

	(push-scope)
	(loop-start)
		;read mail command
		(call 'sys_mail :mymail :nil {msg, data})
		(breakifnot {msg->msg_frag_length})

		;sample reply
		(call 'sys_mail :alloc {reply_size} {reply, rdata})
		(assign {data->net_id_mbox_id} {reply->msg_dest.net_id_mbox_id})
		(assign {data->net_id_node_id.node_id_node1}
			{reply->msg_dest.net_id_node_id.node_id_node1})
		(assign {data->net_id_node_id.node_id_node2}
			{reply->msg_dest.net_id_node_id.node_id_node2})
		(call 'sys_kernel :id :nil {
			rdata->reply_node_id.node_id_node1,
			rdata->reply_node_id.node_id_node2})
		(call 'sys_task :count :nil {rdata->reply_task_count})
		(call 'sys_mem :used :nil {rdata->reply_mem_used})
		(call 'sys_mail :send {reply})
		(call 'sys_mail :free {msg})

		;be friendly
		(call 'sys_task :sleep '(0))
	(loop-end)

	(call 'sys_mail :free {msg})
	(pop-scope)
	(return)

(def-func-end)
```

## Lisp Interface

The included Lisp interpreter interfaces to the messaging system via sending
and receiving objects. This uses the `'sys_mail :alloc_obj` and `'sys_mail
:free_obj` calls within the Lisp bindings, see `sys/lisp.inc` and
`sys/mail/lisp.vp`.

It is possible to send a Lisp list to a process that lives on the same CPU,
this will just pass an object reference between the processes.

Senders can martial data for sending via a `(str ...)` or `(str (string-stream
...))` and Receivers with a `(string-stream msg)` or combined with a `(read
...)` call on a `(string-stream msg)`, the world is your Lobster have fun.

The Lisp level `(mail-timeout net_id ns user)` function can be used to send a
mail message to a mailbox after a time delay ! This message, on receipt, will
contain the 64bit value of the current time and the 64bit user value. This can
be used for animation callback purposes or in combination with `'sys_mail
:select` to provided timed mailbox read functionality. If the time delay given
is 0 the call will remove the entry from the timer list.

For an example of how you can use `(mail-timeout ...)` to aid UI construction,
take a look at the GUI tooltips ! This makes use of the user value as a View id
to show which GUI widget is requesting tool tip info display.

### Lisp example

In order to ease simple message construction sending and receiving, you can use
the `(structure)` macros in conjunction with raw string allocation and the
field access macros `(getf) (setf) (setf->)` !

Let's look at the Netmon application as an example. This application creates a
child task on each network node by use of the `(Global)` class and polls each
child to request node specific information.

The application defines a polling message structure in the
`apps/netmon/app.inc` file.

```file
apps/netmon/app.inc
```

Looking at the parent task `apps/netmon/app.lisp` it then sends out, at regular
intervals, a polling message to each child task, that consists of the parents
reply mailbox. Note that the `(elem-get +select_reply select)` will just be the
mailbox id string returned from its earlier call to `(mail-alloc-mbox)`.

```vdu
...
(defun poll (key val)
	; (poll key val)
	;function called to poll entry
	(when (defq child (. val :find :child))
		(mail-send child (elem-get +select_reply select))))
...
```

The child task `apps/netmon/child.lisp`, within its event loop, receives and
replies to the parent request by using `(str-alloc)` and `(setf->)` to build
the reply message.

```vdu
...
	(bind '(task_count mem_used) (kernel-stats))
	(mail-send msg (setf-> (str-alloc +reply_size)
		(+reply_node (slice +long_size -1 (task-mailbox)))
		(+reply_task_count task_count)
		(+reply_mem_used mem_used)))
...
```

On receipt of the child's reply the parent unpacks the response using `(getf)`.

```vdu
...
	;child poll response
	(when (defq val (. global_tasks :find (getf msg +reply_node)))
		(defq task_val (getf msg +reply_task_count)
			memory_val (getf msg +reply_mem_used)))
...
```

Here is the full child task source so you can see how it creates multiple
mailboxes and uses `(mail-select) (mail-timeout)` calls to wait for a parent
polling request or time out and exit if orphaned.

```file
apps/netmon/child.lisp
```
