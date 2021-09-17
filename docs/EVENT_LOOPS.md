# Event loops, mailbox selection and strategies

In this document we cover how applications and libraries go about using
mailboxes and messages to create communication structures, what techniques are
used and why.

Each task on creation is allocated a main mailbox. You can create more if
desired, but why would you do that ? What advantage is there ?

* It allows you to partition your application and its messaging in a way that
makes it easier to understand what event just occurred. You don't have to rely
on some way of typing your messages so you can distinguish between them.

* In certain fault tolerant situations, to be able to retry an action without
needing to worry about replies that may come to you from failed or timed out
child tasks.

* In a GUI application event loop the GUI process will send your application
GUI events via the main mailbox and you will need to create additional
mailboxes in order to keep those separate from your other activities.

* Prioritizing messages by using an ordered selection list of mailboxes with
the higher priority messages going to the earlier entries in that selection
list.

Most applications in ChrysaLisp, like Taos before it, are asynchronous
distributed state machines. ChrysaLisp provides the tools (tasks, mailboxes and
messages) for you to create such applications, it does not dictate how you do
them. "The Tao does not do, but nothing is not done !"

## Allocating and freeing multiple mailboxes

The mailbox selection function `(mail-select)` takes a list of local mailbox
network IDs, one for each mailbox, to monitor for incoming messages.

So the first thing we need to do is give them some sensible names. That's
normally done by using the `(enums)` macro. We are simply creating some
constants for the list element index of each mailbox. This just lets us use a
readable symbol, rather than an opaque number, in our source code, which makes
it easier to change later if we want to add more.

Let's look at an example from the `apps/boing/app.lisp` demo to get us started:

```vdu
(enums +select 0
	(enum main timer))
```

This creates these symbols and bound values:

```vdu
+select_main 0
+select_timer 1
+select_size 2
```

We place the `main` mailbox as the first element in order to use built in
helper functions to allocate and free our mailbox selection list. These
functions will always have element 0 as the `(task-mailbox)`, all the remaining
will be allocated and freed for us using the `(mail-alloc-mbox)` and
`(mail-free-mbox)` functions.

At the start of the Boing demo `(main)` function the selection list is created
and at the end it is destroyed:

```vdu
(defun main ()
	(defq select (alloc-select +select_size))
	...
	... event loop
	...
	(free-select select))
```

## Waiting on multiple mailboxes, the event loop

In the Boing demo we have two types of messages possible. We will receive GUI
messages in our `main` mailbox plus we will use the declared `timer` mailbox to
pump our animation routine.

After the mailbox selection is created we ask the system to send us a message,
to the `timer` mailbox !, after a certain time period has passed. Each time we
receive this `frame` time event, we restart the timer and run our animation
code.

We distinguish between the two possible types of message by use of the index
that the `(mail-select)` function returns to us. This function will block until
one of the mailboxes has mail. The first to do so will unblock the call and
return the index of the first mailbox that has mail. The mail message is not
read, just the index is returned so we know what has happened.

We read the message from that selection index mailbox and then decide what
action to take based on the index value:

```vdu
	(mail-timeout (elem +select_timer select) rate 0)
	(while ...
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main)
				;main mailbox
				...
				)
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem +select_timer select) rate 0)
				...
				)))
```

## Transient mailboxes for IPC/RPC

Sometimes you will wish to communicate with another task, possibly a `service`,
in a sequential style of interaction. Send off the request and wait till the
reply then return. You don't wish to use any of your application mailboxes as
this will really mess up the application logic and state machine. So use a
transient mailbox !

Here is the GUI RPC function from `gui/gui/lisp.inc`. This is the function that
your application calls, via the `(gui-add-front)` and `(gui-sub)` macros to add
and remove your application window from the screen.

```vdu
(defun gui-rpc (view cmd)
	(when (/= 0 (length (defq services (mail-enquire "GUI_SERVICE"))))
		(setq services (filter (# (eql
				(slice +long_size -1 (task-mailbox))
				(slice +long_size -1 %0)))
			(map (# (to-net-id (elem 1 (split %0 ",")))) services)))
		(defq mbox (mail-alloc-mbox))
		(mail-send (pop services) (list cmd view (task-mailbox) mbox))
		(mail-read mbox)
		(mail-free-mbox mbox)
		view))

(defmacro gui-sub (view) `(gui-rpc ,view 0))
(defmacro gui-add-front (view) `(gui-rpc ,view 1))
(defmacro gui-add-back (view) `(gui-rpc ,view 2))
```

Don't worry about the service lookup code, just the RPC part:

```vdu
(defq mbox (mail-alloc-mbox))
(mail-send ...)
(mail-read mbox)
(mail-free-mbox mbox)
```

A temp mailbox is allocated, we send off the request to the GUI service, and
tell it to reply to the temp mailbox. Once we receive the reply message we free
the temp mailbox. The act of creating and destroying a temp mailbox is very
fast, no need to worry that this is thrashing the system. There are also other
reasons to destroy an existing mailbox and recreate it as we will cover later.

## Coping with failure

Let's say we have spawned a task out on the network and are waiting for some
reply from a job we posted to it. It may be that we can't be sure that it will
ever reply back to us if somebody is allowed to pull out the link between our
node and that other node ! An application that wished to survive such a failure
event will need to restart the task and retry the job.

Ultimately some kind of timeout gets employed, one way or another somebody has
to decide, "it's not coming". That might be in the link layer or the library
layer or whatever. Here we will show how to make the application immune to the
problem no matter what layer caused the issue.

If we look at the Raymarch demo `apps/raymarch/app.lisp` this consists of a
parent GUI app and a child task that's spawned multiple times to fill the
available nodes. Those child tasks are then `farmed` with jobs from a job que.
Each time a job result comes back the que is drained and a new job is sent out
to that child. Repeat till the job que is empty.

This demo makes use of the `lib/task/farm.inc` library. This library holds a
map of task id to child task records. We expect to receive a reply from a task
within a certain amount of time and if we don't get one then we destroy that
child's task record and restart.

The library handles some housework for us and calls back to `(create)` and
`(destroy)` functions provided by the application.

As before we allocate multiple mailboxes:

```vdu
(enums +select 0
	(enum main task reply timer))
```

* `+select_main` will be our main mailbox for GUI events.

* `+select_task` will be our reply mailbox for a started task. We receive a
message of the child task ID to this mailbox when we start a new child. But
note that the action of starting a task takes time, we send off the task start
request and sometime later, maybe never, we get a reply of the ID !

* `+select_reply` will be our job reply mailbox. After we send off a job the
child task will reply to this mailbox with a result. Not all job replies are
guaranteed to happen !

* `+select_timer` will be our timeout mailbox. We will have this event happen
every so often to pump our retry calls. In this case we will be calling a
method on the Farm library to restart any child tasks that are overdue.

Let's not get bogged down in all the specifics of this application but
concentrate on what happens when we get the callbacks from the library and how
to send off a job.

```vdu
(defun dispatch-job (key val)
	;send another job to child
	(cond
		((defq job (pop jobs))
			(.-> val
				(:insert :job job)
				(:insert :timestamp (pii-time)))
			(mail-send (. val :find :child)
				(setf-> job
					(+job_key key)
					(+job_reply (elem +select_reply select)))))
		(t  ;no jobs in que
			(.-> val
				(:erase :job)
				(:erase :timestamp)))))

(defun create (key val nodes)
	; (create key val nodes)
	;function called when entry is created
	(open-task "apps/raymarch/child.lisp" (elem (random (length nodes)) nodes)
		+kn_call_child key (elem +select_task select)))

(defun destroy (key val)
	; (destroy key val)
	;function called when entry is destroyed
	(when (defq child (. val :find :child))
		(mail-send child ""))
	(when (defq job (. val :find :job))
		(push jobs job)
		(. val :erase :job)))
```

When the library wishes to create a new child task, it will call our `(create)`
function. The `key` parameter will be the assigned task handle, not a mailbox
ID, just a number we use to look up this tasks record. The `val` parameter is
an `emap` object, one per child, where we store information to track this
child's state. The `nodes` parameter is the list of currently known network
nodes, we get to choose where we will open the child task from this list.

Here we pick a network node at random and send off an open task request to that
node, when the reply comes in it will be to the `+select_task` mailbox and
contain the `key` so we can retrieve the child's record.

When the library wishes to destroy a child task, it will call our `(destroy)`
function. Here we look to see if we ever received the tasks network ID, and if
so send off a `quit` message to it. Then look to see if it had an outstanding
job request, and if it did, we push the request back onto the job que.

The `(dispatch-job)` function will be used below in the event loop to dispatch
a new job to any newly started child task, as they report in, or issue a new
job as we receive a result.

A farm is created that's twice as big as the known number of network nodes.
Roughly two child tasks will exist per node. Remember that ChrysaLisp does the
final task distribution, in this demo we only suggest the node to start the
task.

```vdu
(defun main ()
	(defq select (alloc-select +select_size))
	...
	(defq farm (Farm create destroy (* 2 (length (mail-nodes)))))
	(mail-timeout (elem +select_timer select) timer_rate 0)
	(while ...
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_main)
				;main mailbox
				...
				)
			((= idx +select_task)
				;child launch responce
				(defq key (getf msg +kn_msg_key)
					  child (getf msg +kn_msg_reply_id))
				(when (defq val (. farm :find key))
					(. val :insert :child child)
					(dispatch-job key val)))
			((= idx +select_reply)
				;child responce
				(defq key (get-long msg (- (length msg) +long_size)))
				(when (defq val (. farm :find key))
					(dispatch-job key val))
				(setq dirty t)
				...
				)
			(t  ;timer event
				(mail-timeout (elem +select_timer select) timer_rate 0)
				(. farm :refresh retry_timeout)
				(when dirty
					(setq dirty nil)
					...
					(when (= 0 (length jobs))
						(defq working nil)
						(. farm :each (lambda (key val)
							(setq working (or working (. val :find :job)))))
						(unless working (. farm :close)))))))
	...
	(. farm :close)
	(free-select select))

```

I've cut out the specifics of what we do in this application with the job
results to concentrate on the task management and job code.

* When we get a child task launch response the network ID is recorded and its
first job is dispatched.

* When we get a child task job response, it has a new job dispatched.

* When the retry timer expires we reset the timer, call the farm `:refresh`
method, details below, and close the farm if all the jobs are finished.

The Farm class is listed here:

```file
lib/task/farm.inc
```

The `:refresh` method scans through the farm and any child task that has an
outstanding job that's overdue is restarted, ie. `(destroy)` and `(create)` are
called.

In this way, the application will have all the jobs processed, any that die for
whatever reason will be restarted and the outstanding jobs won't be lost.

The child task is also robust in that it will self quit if it is orphaned. Too
long to receive some work, 5 seconds here, and it goes away.

```vdu
(enums +select 0
	(enum main timeout))

...

(defun main ()
	(defq select (alloc-select +select_size) running t +timeout 5000000)
	(while running
		(mail-timeout (elem +select_timeout select) +timeout 0)
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with result
				(mail-timeout (elem +select_timeout select) 0 0)
				(defq key (getf msg +job_key)
					mbox (getf msg +job_reply)
					msg (slice +job_x -1 msg))
				...)))
	(free-select select))
```

## Coping with stale messages

As mentioned earlier there is a good reason to destroy and recreate a mailbox.
Mailboxes are assigned a none repeating ID. Each new mailbox has a new unique
ID and any message sent to an old destroyed mailbox with a destination ID that
can't be validated to exist is discarded.

The Raymarch demo didn't have this problem because it runs once to create the
scene display. But the Mandelbrot demo also uses a farm and the user CAN
restart the calculations for a new position, in order to zoom in and out, at
any point !

The Mandelbrot demo `apps/mandelbrot/app.lisp` has a `(reset)` function defined
that recreates the farm, job que and the `+select_reply` mailbox of the
selection in order to safely ignore any `in flight` messages to the old
mailbox.

```vdu
(defun reset ()
	(if farm (. farm :close))
	(mail-free-mbox (elem +select_reply select))
	(elem-set +select_reply select (mail-alloc-mbox))
	(setq jobs (map (lambda (y)
			(setf-> (str-alloc +job_size)
				(+job_x 0)
				(+job_y y)
				(+job_x1 (* canvas_width canvas_scale))
				(+job_y1 (inc y))
				(+job_w (* canvas_width canvas_scale))
				(+job_h (* canvas_height canvas_scale))
				(+job_cx center_x)
				(+job_cy center_y)
				(+job_z zoom)))
			(range (dec (* canvas_height canvas_scale)) -1))
		farm (Farm create destroy (* 2 (length (mail-nodes))))))
```

## Delayed actions with `(mail-timeout)`

So far we have only used `(mail-timeout)` to pump animation and retry code. But
we can use it for more than that.

The GUI tooltips use this function to pop up a `tip` bubble for toolbar
buttons. Properties are set on the application window object that will be used
by the tooltip code.

This example is from the `apps/bubbles/app.lisp`, Bubbles application:

```vdu
(enums +select 0
	(enum main timer tip))

...

(defun tooltips ()
	(def *window* :tip_mbox (elem +select_tip select))
	(each (# (def %0 :tip_text %1)) (. main_toolbar :children)
		'("refresh"))
	(each (# (def %0 :tip_text %1)) (. style_toolbar :children)
		'("plain" "grid" "axis")))
```

We declare an extra selection mailbox to be used by the tip events, create a
`:tip_mbox` property on the root window and add `:tip_text` properties to each
of the toolbar buttons.

The `:mouse_enter` method of the Button class uses `(mail-timeout)` to create a
delayed message to the `:tip_mbox`, ie. our `+select_tip` mailbox. It uses the
id parameter of the timed mail event to hold the button object id, this is so
when the event is processed in the event loop of the application we can use
this to find which button wants the tip shown.

Here is the Button class `:mouse_enter` method:

```vdu
(defmethod :mouse_enter (this event)
	; (. button :mouse_enter event) -> button
	(and (def? :tip_text this) (defq tip_mbox (get :tip_mbox this))
		(mail-timeout tip_mbox 1000000 (. this :get_id)))
	this)
```

As the mouse enters a button instance it tests to see if a `:tip_text` property
is defined on the instance. If so AND `:tip_mbox` is defined in the UI tree,
remember we added this to our root window, it creates the timed mail event.

And here is the event loop case in the Bubbles `(main)` function:

```vdu
((= idx +select_tip)
	;tip time mail
	(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
		(. view :show_tip)))
```
