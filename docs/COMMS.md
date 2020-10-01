# Communications

In ChrysaLisp while it is not forbidden for processes to communicate with
shared memory structures, provided they live on the same CPU, the main means
that processes use to communicate with each other is via message passing. Each
process on creation is allocated a main mailbox, processes create messages and
send these messages between themselves by use of these mailboxes. More
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
mail and sent to this CPUs Postman task. This postman task just currently frees
the message but later versions of the system may use this information for
debugging purposes.

## Messages

Messages consist of two parts, the message header and the message body. The
message body consists of a referenceable object, most often a String object.
The header is like a postage stamp on the objects data. Large objects will have
multiple message headers created, by the system, to break up the sending of the
data into smaller packets. These packets are routed from the source CPU to the
destination CPU and reconstructed at the destination into the original message
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

A 64 bit Process mailbox ID consists of a combination of the local mailbox ID
and the CPU ID. The CPU ID occupies the upper 32 bits, the local mailbox ID
occupies the lower 32 bits.

Message routing first of all routes messages from the source CPU to the
destination CPU and then the local mailbox ID is validated before delivery of
the message into the receivers mailbox. Any message with an invalid local
mailbox ID is discarded as junk mail.

On creation of a process it is allocated a main local mailbox and the 64 bit
mailbox ID is returned to the creator. There are functions provided to create
multiple child processes in a single call. Such functions can be used to create
farms, arrays, and pipelines of processes. These functions return an array of
64 bit mailboxes IDs. The creator can then go on to wire these mailboxes and
processes in any communications structure it desires.

Auto allocated main process ID mailboxes do not need to be manually freed, this
will happen when the process shuts down.

## Services

Functions are also provided to allow mailboxes to be named, via `'sys_mail
:declare` throughout the network. Such mailboxes are then discoverable by other
processes via a call to `'sys_mail :enquire` with the given name. A service
entry can be removed byt the with the `'sys_mail :forget` function.

The system maintains a directory of these service names and the corresponding
process mailbox IDs. An example service is the current GUI `DEBUG_SERVICE`
application `apps/debug/app.lisp`.

### Example

This is the network monitoring application child process
`apps/netmon/child.vp`. This process is launched onto each CPU by use of an
`'sys_task :openfarm` call. The child process simply waits for a command
message from the parent and either exits or returns a message containing task
and memory usage information.

`'sys_mail :mymail` is just a convenience function to read mail from the
current tasks main mailbox.

```lisp
(include "sys/func.inc")
(include "sys/kernel/class.inc")

(def-struct 'sample)
	(ulong 'id)
(def-struct-end)

(def-struct 'sample_reply)
	(uint 'cpu 'task_count 'mem_used)
(def-struct-end)

(def-func 'apps/netmon/child)
	;monitor task

	(ptr 'msg 'data 'reply 'rdata)

	(push-scope)
	(loop-start)
		;read mail command
		(call 'sys_mail :mymail nil {msg, data})
		(breakifnot {data->sample_id})

		;sample reply
		(call 'sys_mail :alloc {sample_reply_size} {reply, rdata})
		(assign {data->sample_id} {reply->msg_dest.id_id})
		(call 'sys_kernel :id nil {rdata->sample_reply_cpu})
		(call 'sys_task :count nil {rdata->sample_reply_task_count})
		(call 'sys_mem :used nil {rdata->sample_reply_mem_used})
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

Arrays and Strings can be used as message data and these will always be
received as String objects, even if the destination is on the same CPU. Arrays
will have their data copied into a String object before sending.

Senders can martial data for sending via a `(str ...)` or `(str (string-stream
...))` and Receivers with a `(string-stream msg)` or combined with a `(read
...)` call on a `(string-stream msg)`, the world is your Lobster have fun.
