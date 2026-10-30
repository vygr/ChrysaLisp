;import symbols
(import "./kernel/struct.inc")
(import "./list/struct.inc")
(import "./task/struct.inc")
(import "./mail/struct.inc")
(import "./pii/struct.inc")

(defun mail-entry-id (entry)
	; (mail-entry-id entry) -> net_id
	;the mailbox of an entry as (mail-enquire) gives them, name,id,info
	(hex-decode (second (split entry ","))))

(defun mail-first (name)
	; (mail-first name) -> :nil | net_id
	;the mailbox of the first said as name, (mail-declare), wherever it is
	(if (nempty? (defq found (mail-enquire (cat name ",")))) (mail-entry-id (first found))))

(defun mail-lowest (name)
	; (mail-lowest name) -> :nil | net_id
	;the lowest mailbox said as name. A service that is to be one may be
	;started twice at once, by two that each found none: every task that
	;sees both picks the same this way, the one that stays, (mail-single)
	(if (nempty? (defq found (mail-enquire (cat name ","))))
		(hex-decode (reduce (# (if (< (cmp %1 %0) 0) %1 %0))
			(map (# (second (split %0 ","))) found)))))

(defun mail-single (name mbox)
	; (mail-single name mbox) -> :nil | net_id
	;:nil if the service said as name with this mailbox is the one, else
	;the one it is to give way to, the lowest. The names come to each node
	;by its pings, so a service checks each time it wakes; one that has to
	;give way stops saying its name, sends on what still comes to it, and
	;goes. Only for a service a task may use any one of: two networks on
	;one machine each have a Net service of their own
	(unless (eql (defq lowest (mail-lowest name)) mbox) lowest))

(import "service/lock/app.inc")

(defun task-nodeid (&optional mbox)
	; (task-nodeid [mbox]) -> nodeid
	(setd mbox (task-mbox))
	(slice mbox +mailbox_id_size -1))

(defun mail-here (name)
	; (mail-here name) -> :nil | net_id
	;one said as name, (mail-declare), on this node, whose task is still
	;there: one that died without (mail-forget) is still said, its mailbox
	;is gone. Each desktop is a node, so this is the one of this desktop,
	;the guard of a service or an app there is to be one of
	(defq node (task-nodeid))
	(some (lambda (entry)
			(defq id (mail-entry-id entry))
			(and (eql (task-nodeid id) node) (mail-validate id) id))
		(mail-enquire (cat name ","))))

(defun mail-service (name)
	; (mail-service name) -> :nil | net_id
	;the service said as name to use: the one on this node, the desktop's,
	;if it is still there, else one on another node. A task on a node with
	;no desktop has any one on the machine. One on this node that is not
	;here is dead; one on another can not be asked, (mail-validate) knows
	;only the mailboxes of the node it is asked on
	(or (mail-here name)
		(some (lambda (entry)
				(defq id (mail-entry-id entry))
				(if (nql (task-nodeid id) (task-nodeid)) id))
			(mail-enquire (cat name ",")))))

(defun task-mboxes (size)
	; (task-mboxes size) -> ((task-mbox) [temp_mbox] ...)
	(defq select (list (task-mbox)))
	(while (> (-- size) 0) (push select (mail-mbox)))
	select)

(defun open-task (task node mode key_num reply)
	; (open-task task node mode key_num reply)
	(mail-send (cat (char 0 +long_size) node)
		(cat (char mode +long_size) (char key_num +long_size)
			reply task (char 0))))

(defun open-child (task mode)
	; (open-child task mode) -> net_id
	(defq mbox (mail-mbox))
	(open-task task (task-nodeid) mode 0 mbox)
	(getf (mail-read mbox) +kn_msg_reply_id))

(defun open-remote (task node mode)
	; (open-remote task node mode) -> net_id
	(defq mbox (mail-mbox))
	(open-task task node mode 0 mbox)
	(getf (mail-read mbox) +kn_msg_reply_id))

(defun open-pipe (tasks &optional modes)
	; (open-pipe tasks [modes]) -> ([net_id | 0] ...)
	(setd modes (list))
	(defq ids (cap (length tasks) (list)) mbox (mail-mbox) index -1 tail (task-mbox))
	(while (< (++ index) (length tasks))
		(open-task (elem-get tasks index) (slice tail +long_size -1)
			(if (< index (length modes)) (elem-get modes index) +kn_call_run) 0 mbox)
		(defq tail (getf (mail-read mbox) +kn_msg_reply_id))
		(push ids tail)
		(if (= 0 (get-long tail 0)) (setq tail (task-mbox))))
	ids)

(defun task-timeout (time)
	; (task-timeout s) -> ns
	(if (starts-with "obj/vp64" (load-path)) (* time 10000000) (* time 1000000)))

(defun node-link (name)
	; (node-link name) -> net_id
	;start a shared memory link on this node. The node at the other end
	;starts one of the same name.
	(defq link (open-child "sys/link/link" +kn_call_pin))
	(if (/= 0 (get-long link 0)) (mail-send link (cat name (char 0))))
	link)

(defun node-start (total pairs &optional fronts kind script net sid)
	; (node-start total pairs [fronts kind script net sid]) -> (pid ...)
	;this node is node 0 of total, start the others, 1 on, linked as pairs
	;has them, each a list of two node numbers. A pid of -1 is a node the
	;host could not start. The first fronts of them, all if not given, are
	;of kind, :gui, :gui_hidden or :tui, and run script. Every node is the
	;one host program, which has a GUI if its machine has. A node with a
	;script is a front of its session, a way in to it, a desktop is
	;service/gui/app.lisp. net is a line to head
	;the note of them with, so they can be found and stopped as one,
	;(node-nets) and (node-stop). sid is a system id for them, in place
	;of this machine's, 16 characters, the kernel's -sid option.
	(setd fronts total)
	(defq links (map (lambda (&) (list)) (range 0 total)) names (list)
		chars "0123456789abcdefghijklmnopqrstuvwxyz"
		;a GUI that is never seen, its window hidden, -hidden, for a test
		host (if (eql kind :gui_hidden) "-hidden " "")
		;a session can have a user of its own, not the one the machine
		;has. Each node of it has who in *env_node_user*, in the
		;environment all its tasks share, and tells the nodes it starts as
		;the first thing they run. A node's args are split at spaces, and
		;on Windows at tabs as well, so the form is written with neither,
		;a vertical tab, which the reader takes for white space as it does
		;anything up to a space. A hidden desktop of a session with no user
		;of its own is the machine's user, usr/current, read and not
		;written: it has no one to ask, its login window is never seen, and
		;its services, Audio with them, start only once it knows
		user (ifn (get '*env_node_user*)
			(if (eql kind :gui_hidden) (trim (ifn (load "usr/current") "Guest"))))
		tab "\v"
		;and so with a system id that is not the machine's. A node of such
		;a system has it in *env_node_sid*, the nodes it starts are of the
		;same system, and its Net service knows it is not the machine's
		;own and keeps off the network, service/net/app_impl.lisp
		sid (ifn sid (get '*env_node_sid*))
		;and with a session that keeps to itself, that does not join the
		;mesh unasked, *env_node_solo*
		solo (get '*env_node_solo*)
		tell (if (or user sid solo) (cat "(def" tab "*root_env*"
			(if user (cat tab "'*env_node_user*" tab "{" user "}") "")
			(if sid (cat tab "'*env_node_sid*" tab "{" sid "}") "")
			(if solo (cat tab "'*env_node_solo*" tab ":t") "") ")"))
		told (if tell (cat " -run " tell) "")
		run (cond
			((and script tell) (cat " -run (progn" tab tell tab
				(if (starts-with "(" script) script (cat "(import" tab "{" script "})")) ")"))
			(script (cat " -run " script))
			(:t told)))
	(each (lambda ((i j))
			;a link name is ???-???, the form the stop scripts clear away
			(defq name (apply (const cat) (map (lambda (_)
				(if (= _ 3) "-" (elem-get chars (random 36)))) (range 0 7))))
			(push names name)
			(push (elem-get links i) name)
			(push (elem-get links j) name))
		pairs)
	(defq pids (map! (lambda (node_links)
			(defq args (cat (if sid (cat "-sid " sid " ") "")
				(join (map (lambda (name) (cat "-l " name)) node_links) " ")))
			(pii-spawn (if (<= (!) fronts) (cat host args run) (cat args told))))
		(list links) 1))
	(each (const node-link) (first links))
	;leave the pids and the link names for the launch script that started
	;this node's session, it stops what its nodes started, see funcs.sh
	(unless (eql (os) 'Windows)
		;the first note this node leaves is a new file. One of that name
		;that is there is some other process's of long ago, with the
		;number this one has now, and names what is not this node's
		(defq file (cat "/tmp/chrysalisp_" (str (pii-pid)) ".session")
			noted (get '*node_noted*)
			text (cat (if (and noted (/= (age file) 0)) (load file) "")
				(if net (cat "net " net "\n") "")
				(join (map (lambda (pid)
					(if (> pid 0) (cat (if (and script (< (!) fronts)) "front " "pid ")
						(str pid) "\n") "")) pids) "")
				(join (map (# (cat "link " %0 "\n")) names) "")))
		(def *root_env* '*node_noted* :t)
		;a node that started nothing leaves no note, there is nothing to
		;stop or clear away for it. An empty one was left by every node
		;that came up alone, and by no launch script was it cleared: six
		;a run of the tests. One of that name from long ago still goes
		(cond
			((nempty? text) (save text file))
			((/= (age file) 0) (pii-remove file))))
	pids)

(defun node-wait (want)
	; (node-wait want)
	;wait till that many nodes are seen, so what is started next can
	;spread over them, and not for ever
	(defq t0 (pii-time))
	(while (and (< (length (lisp-nodes)) want) (< (- (pii-time) t0) (task-timeout 10)))
		(task-sleep 10000)))

(defun node-shape (shape &optional cnt)
	; (node-shape shape [cnt]) -> (total pairs)
	;the links of a network of a shape, each a list of two node numbers.
	;:full, every node to every other, :ring, :star, node 0 in the middle,
	;:tree, two below each, :mesh, a square that wraps round, and :cube. cnt
	;is the number of nodes, the width for a :mesh or a :cube. Not given, or
	;0, it is sized to this machine, the most that is no more than one for
	;each processor. No more than 64 nodes, 32 for :full.
	(defq wide (case shape (:mesh 2) (:cube 3) (:t 1))
		most (if (eql shape :full) 32 64) keys (list) pairs (list)
		pair (lambda (i j)
			(defq key (+ (* (min i j) 256) (max i j)))
			(unless (or (= i j) (find key keys))
				(push keys key)
				(push pairs (list (min i j) (max i j))))))
	(unless (and cnt (> cnt 0))
		(setq cnt 1 most (min most (pii-cpus)))
		(while (<= (pow (inc cnt) wide) most) (++ cnt)))
	(while (> (pow cnt wide) most) (-- cnt))
	(defq total (pow cnt wide))
	(case shape
		(:ring (each (# (pair %0 (% (inc %0) total))) (range 0 total)))
		(:star (each (# (pair 0 %0)) (range 1 total)))
		(:tree (each (# (pair %0 (/ (dec %0) 2))) (range 1 total)))
		(:mesh (each (lambda (y) (each (lambda (x)
				(defq node (+ (* y cnt) x))
				(pair node (+ (* y cnt) (% (inc x) cnt)))
				(pair node (+ (* (% (inc y) cnt) cnt) x)))
			(range 0 cnt))) (range 0 cnt)))
		(:cube (each (lambda (z) (each (lambda (y) (each (lambda (x)
				(defq node (+ (* z cnt cnt) (* y cnt) x))
				(pair node (+ (* z cnt cnt) (* y cnt) (% (inc x) cnt)))
				(pair node (+ (* z cnt cnt) (* (% (inc y) cnt) cnt) x))
				(pair node (+ (* (% (inc z) cnt) cnt cnt) (* y cnt) x)))
			(range 0 cnt))) (range 0 cnt))) (range 0 cnt)))
		(:t (each (lambda (i) (each (lambda (j) (pair i j))
			(range (inc i) total))) (range 0 total))))
	(list total pairs))

(defun node-spawn (&optional num kind script)
	; (node-spawn [num kind script]) -> (pid ...)
	;start num more nodes on this machine, default 1, each linked to this
	;node and to each other. kind and script are as (node-start) has them,
	;so a GUI node can be added to a TUI network, and a TUI node to a GUI
	;one.
	(setd num 1)
	(if (< num 1) (list)
		(node-start (inc num) (second (node-shape :full (inc num))) :nil kind script)))

(defun node-net (shape &optional cnt fronts kind script id sid)
	; (node-net shape [cnt fronts kind script id sid]) -> (pid ...)
	;make this node node 0 of a network of a shape, (node-shape) has them
	;and what cnt is, and wait for the nodes to be seen. It is how a
	;session starts, from one node, and how a network is added to one
	;that is running, hung from this node. The first fronts of them,
	;default none, are of kind and run script, as (node-start) has it,
	;the other desktops of a session. With an id, a name for these nodes,
	;they are noted under it, to be stopped as one, (node-stop). With a
	;sid, a system id, they are a system of their own: this node is not
	;one of the shape, the whole of it is new nodes with that id, and
	;one link from here to the first of them is the way in.
	(bind '(total pairs) (node-shape shape cnt))
	(when sid
		(setq pairs (cat (list (list 0 1)) (map (lambda ((i j)) (list (inc i) (inc j))) pairs))
			total (inc total)))
	(defq before (length (lisp-nodes))
		pids (node-start total pairs (ifn fronts 0) kind script
			(if id (cat id " " (rest (str shape)) " " (str (if sid (dec total) total))
				(if sid (cat " " sid) ""))) sid))
	(node-wait (+ before (length (filter (lambda (pid) (> pid 0)) pids))))
	pids)

(defun node-auto (&optional per_cpu)
	; (node-auto [per_cpu]) -> (pid ...)
	;size the network to this machine. Start nodes till there are per_cpu
	;for each processor, default 1, and no more than 32, then wait for
	;them to be seen, so what is started next can spread over them. One
	;for each is where a full build is quickest, more only share them out.
	(setd per_cpu 1)
	(defq before (length (lisp-nodes))
		pids (node-spawn (max 0 (- (min 32 (* per_cpu (pii-cpus))) before))))
	(node-wait (+ before (length (filter (lambda (pid) (> pid 0)) pids))))
	pids)

(defun mail-read-timeout (mbox &optional timeout)
	; (mail-read-timeout mbox [timeout_us]) -> msg | :nil
	(defq timer (mail-timeout (mail-mbox) (ifn timeout (task-timeout 1)) 0))
	(when (= (mail-select (list mbox timer)) 0)
		;mail came first, so cancel the timer
		(mail-timeout timer 0 0)
		(mail-read mbox)))

(defq mail-timeout-read mail-read-timeout)

(defun node-nets ()
	; (node-nets) -> ((id shape total (pid ...) (link ...) file sid) ...)
	;the networks added to this machine's sessions by name, (node-net)
	;with an id, each with the processes of its nodes, the names of its
	;links, the note it is in, and its system id, :nil if it has this
	;machine's. Whichever node of the machine started them
	(defq out (list))
	(each! (lambda (entry kind)
		(when (and (eql kind "f") (starts-with "chrysalisp_" entry) (ends-with ".session" entry))
			(defq file (cat "/tmp/" entry) net :nil)
			(each (lambda (line)
				(defq parts (split line " "))
				(cond
					((empty? parts))
					((eql (first parts) "net")
						(setq net (if (>= (length parts) 4)
							(list (second parts) (third parts) (str-as-num (elem-get parts 3)) (list) (list) file
								(if (> (length parts) 4) (elem-get parts 4)))))
						(if net (push out net)))
					((not net))
					((eql (first parts) "link") (push (elem-get net 4) (second parts)))
					(:t (push (elem-get net 3) (str-as-num (second parts))))))
				(split (ifn (load file) "") "\n"))))
		(dir-list "/tmp"))
	out)

(defun pii-kill (pid)
	; (pii-kill pid) -> :t | :nil
	;end a process that can not be asked to go, a node that does not
	;answer. :t if it was told to end or was not there. The host has to be
	;one that can, it says how new it is in the fourth word of (pii-host),
	;and with one that is older nothing is done. The native function is
	;looked up here, when it is called, and not in class/lisp/root.inc: a
	;boot image of before it was written still starts
	(and (>= (length (defq host (split (pii-host) " "))) 4)
		(>= (str-as-num (elem-get host 3)) 2)
		((ffi "sys/pii/lisp_kill") pid)))

(defun node-leave ()
	; (node-leave) -> :nil | num
	;as this node goes, the nodes it started, and those they started, the
	;tree of their notes, stopped, and their notes and links cleared away,
	;unless a front among them, a desktop or a terminal, is still up: the
	;session lives on in that. A desktop the member started that started
	;nodes of its own, nodes -a in a terminal on it, takes them with it. :nil
	;if nothing went, else how many were told to. Each node of this machine
	;is sent the processes that are to go, and goes if it is one; what is
	;still up a moment on is ended by the host
	(unless (eql (os) 'Windows)
		(defq todo (list (pii-pid)) seen (list) pids (list) links (list) files (list) front :nil)
		(while (defq pid (pop todo))
			(unless (find pid seen)
				(push seen pid)
				(defq file (cat "/tmp/chrysalisp_" (str pid) ".session"))
				(when (defq text (load file))
					(push files file)
					(each (lambda (line)
						(defq parts (split line " "))
						(cond
							((< (length parts) 2))
							((eql (first parts) "link") (push links (second parts)))
							((find (first parts) '("pid" "front"))
								(defq one (str-as-num (second parts)))
								(and (eql (first parts) "front") (pii-alive one) (setq front :t))
								(push pids one)
								(push todo one))))
						(split text "\n")))))
		(unless (or front (empty? pids))
			(each (# (open-task (str `(if (find (pii-pid) (quote ,pids)) (pii-exit)))
				%0 +kn_call_pin 0 (mail-mbox))) (lisp-nodes :t))
			(task-sleep (/ (task-timeout 1) 2))
			(each (# (if (pii-alive %0) (pii-kill %0))) pids)
			(each (# (pii-remove (cat "/tmp/" %0))) links)
			(each (const pii-remove) files)
			(length pids))))

;the bots of this node, and when they all rest next, and till when: a rest
;is of every bot of the node at once, one bot that rests while another
;works leaves the apps no turn. And the function that says if an app is
;ready, looked up the first time it is wanted
(defq *bot_breaths* (Fmap 11) +bot_work 20000 +bot_rest 1000 *bot_next* 0 *bot_rest_end* 0
	*bot_apps* :nil)

(defun task-band (band)
	; (task-band :bot | :app) -> :t
	;the band the task runs in. A bot is a task that works by itself for
	;others, a Storage service, its tender, a gopher: one that is ready
	;runs before any app, and after the GUI. sys/task/class.inc, where the
	;numbers 4 and 5 are tk_bot_priority and tk_app_priority. The function
	;is looked up when it is called, not in root.inc, so a boot image of
	;before it was written still starts
	(if (eql band :bot)
		(. *bot_breaths* :insert (task-mbox) (pii-time))
		(. *bot_breaths* :erase (task-mbox)))
	((ffi "sys/task/lisp_priority") (if (eql band :bot) 4 5)))

(defun bot-breathe (now)
	; (bot-breathe now)
	;(task-breathe) of a task that is a bot, given the time: every bot of
	;the node rests at once, from the time it is due till +bot_rest after,
	;and one that comes to it in that time rests to its end
	;and only if an app of the node is ready, there is nobody to rest for
	(cond
		((>= now *bot_next*)
			(setq *bot_next* (+ now +bot_work))
			(unless *bot_apps* (setq *bot_apps* (ffi "sys/task/lisp_apps")))
			(cond
				((*bot_apps*)
					(setq *bot_rest_end* (+ now +bot_rest))
					(task-sleep +bot_rest))
				((task-slice))))
		((< now *bot_rest_end*) (task-sleep (- *bot_rest_end* now)))
		((task-slice))))

(defun task-breathe ()
	; (task-breathe)
	;(task-slice), and for a bot, every so often, +bot_work, a rest of all
	;the bots of the node, +bot_rest, so the apps of the node have a turn:
	;a bot gives way only to its own band and those before it, and one at
	;work for a second held every app of its node for that second.
	;Measured, 20 gophers that never came back, the apps waited 9.1 s
	(if (. *bot_breaths* :find (task-mbox)) (bot-breathe (pii-time)) (task-slice)))

(defun node-stop (id)
	; (node-stop id) -> :nil | num
	;stop the nodes of a network that was added by name, all of them, and
	;clear away its links and the note of it. :nil if there is none of
	;that name, else how many nodes are to go. Each node of this machine
	;is sent a task that exits it if it is one of them, not at once: the one
	;that asks may itself be a node of the network, and is given the time
	;to say what it did
	(when (some (# (eql (first %0) id)) (defq nets (node-nets)))
		;a network that one of these nodes added is hung from it, and would
		;be left with no way in, so it goes first, and so for one hung from
		;that. They are found with a list of those still to look at, this
		;does not call itself. The note of a network is named for the
		;process of the node that added it
		(defq found (list) todo (list id) told 0)
		(while (defq name (pop todo))
			(when (and (not (find name found))
					(defq net (some (# (if (eql (first %0) name) %0)) nets)))
				(push found name)
				(each (lambda ((other & & & & its_file &))
					(if (find (str-as-num (slice its_file (const (length "/tmp/chrysalisp_"))
							(- -1 (const (length ".session"))))) (elem-get net 3))
						(push todo other)))
					nets)))
		;the last found are the furthest out, and go first
		(each! (lambda (name)
			(when (defq net (some (# (if (eql (first %0) name) %0)) (node-nets)))
				(bind '(& & & pids links file sid) net)
				(defq id name)
				;every node of this machine, and of the network's system if it is
				;one of its own, is sent the processes that are to go, and one that
				;finds it is one of them goes, in a moment, by itself. They are told
				;now, while every one of them is up to pass it on: a node that goes
				;cuts off those the mail went through it to reach, the far side of
				;a ring was left running. None is asked which it is and waited on,
				;a node that has just gone is still known of, and never answers
				(defq wait (/ (task-timeout 1) 2))
				(each (# (open-task (str `(if (find (pii-pid) (quote ,pids)) (progn (task-sleep ,wait) (pii-exit))))
					%0 +kn_call_pin 0 (mail-mbox))) (cat (lisp-nodes :t) (if sid (lisp-nodes sid) (list))))
				;one that could not be told, it is cut off or it is stuck, is
				;still there when the rest have gone. A task here ends it, in a
				;while, by the host, the caller does not wait for that
				(open-task (str `(progn (task-sleep ,(* wait 5))
						(each (# (if (pii-alive %0) (pii-kill %0))) (quote ,pids))))
					(task-nodeid) +kn_call_pin 0 (mail-mbox))
				(each (# (pii-remove (cat "/tmp/" %0))) links)
				;the note, less this network's lines
				;each line that stays ends as it did, what is noted next goes on
				;the end of the file and would run on from a last line that did not
				;and with nothing left in it the note goes too, an empty one
				;was left for each network stopped by name
				(defq keep :t left (apply (const cat) (cat (list "") (map (# (cat %0 "\n")) (filter (lambda (line)
						(cond
							((empty? line) :nil)
							((starts-with "net " line) (setq keep (nql (second (split line " ")) id)))
							(keep)))
					(split (ifn (load file) "") "\n"))))))
				(if (nempty? left) (save left file) (pii-remove file))
				(setq told (+ told (length pids)))))
			(list found) -1 0)
		told))

(defun net-quiet (&optional delay count last_cnt)
	; (net-quiet [delay_us] [stable_count] [last_cnt]) -> (node_id ...)
	;wait till the number of nodes known has stayed the same for a while.
	;A network that never settles must not hang the caller, so it gives
	;up, with what it has, after 20 times as long as a settled one takes.
	(setd delay 100000 count 8 last_cnt 0)
	(defq stable 0 tries (* count 20))
	(while (and (< stable count) (> (-- tries) 0))
		(task-sleep delay)
		(defq cnt (length (lisp-nodes)))
		(if (and (> cnt 0) (= cnt last_cnt))
			(setq stable (inc stable))
			(setq stable 0 last_cnt cnt)))
	(lisp-nodes))

