;import symbols
(import "./kernel/struct.inc")
(import "./list/struct.inc")
(import "./task/struct.inc")
(import "./mail/struct.inc")
(import "./pii/struct.inc")
(import "service/lock/app.inc")

(defun task-nodeid (&optional mbox)
	; (task-nodeid [mbox]) -> nodeid
	(setd mbox (task-mbox))
	(slice mbox +mailbox_id_size -1))

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
	;of kind, the host program, :gui or :tui, this node's own if not given,
	;and run script. A node with a script is a front of its session, a way
	;in to it, a desktop is service/gui/app.lisp. net is a line to head
	;the note of them with, so they can be found and stopped as one,
	;(node-nets) and (node-stop). sid is a system id for them, in place
	;of this machine's, 16 characters, the kernel's -sid option.
	(setd fronts total)
	(defq links (map (lambda (&) (list)) (range 0 total)) names (list)
		chars "0123456789abcdefghijklmnopqrstuvwxyz"
		host (case kind (:gui "-gui ") (:tui "-tui ") (:t ""))
		run (if script (cat " -run " script) ""))
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
			(pii-spawn (if (<= (!) fronts) (cat host args run) args)))
		(list links) 1))
	(each (const node-link) (first links))
	;leave the pids and the link names for the launch script that started
	;this node's session, it stops what its nodes started, see funcs.sh
	(unless (eql (os) 'Windows)
		(defq file (cat "/tmp/chrysalisp_" (str (pii-pid)) ".session"))
		(save (cat (if (/= (age file) 0) (load file) "")
			(if net (cat "net " net (ascii-char 10)) "")
			(join (map (lambda (pid)
				(if (> pid 0) (cat (if (and script (< (!) fronts)) "front " "pid ")
					(str pid) (ascii-char 10)) "")) pids) "")
			(join (map (# (cat "link " %0 (ascii-char 10))) names) "")) file))
	pids)

(defun node-wait (want)
	; (node-wait want)
	;wait till that many nodes are seen, so what is started next can
	;spread over them, and not for ever
	(defq t0 (pii-time))
	(while (and (< (length (lisp-nodes)) want) (< (- (pii-time) t0) (task-timeout 10)))
		(task-sleep 10000)))

(defun node-spawn (&optional num kind script)
	; (node-spawn [num kind script]) -> (pid ...)
	;start num more nodes on this machine, default 1, each linked to this
	;node and to each other. kind and script are as (node-start) has them,
	;so a GUI node can be added to a TUI network, and a TUI node to a GUI
	;one.
	(setd num 1)
	(if (< num 1) (list)
		(node-start (inc num) (second (node-shape :full (inc num))) :nil kind script)))

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

(defun node-nets ()
	; (node-nets) -> ((id shape total (pid ...) (link ...) file sid) ...)
	;the networks added to this machine's sessions by name, (node-net)
	;with an id, each with the processes of its nodes, the names of its
	;links, the note it is in, and its system id, :nil if it has this
	;machine's. Whichever node of the machine started them
	(defq out (list))
	(each! (lambda (entry kind)
		(when (and (eql kind "8") (starts-with "chrysalisp_" entry) (ends-with ".session" entry))
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
				(split (ifn (load file) "") (ascii-char 10)))))
		(unzip (split (pii-dirlist "/tmp") ",") 2))
	out)

(defun node-stop (id)
	; (node-stop id) -> :nil | num
	;stop the nodes of a network that was added by name, all of them, and
	;clear away its links and the note of it. :nil if there is none of
	;that name, else how many nodes are to go. Each node of this machine
	;is asked which process it is. Those of the network are each sent a
	;task that exits them, not at once: the one that asks may itself be a
	;node of the network, and is given the time to say what it did
	(when (defq nets (node-nets) net (some (# (if (eql (first %0) id) %0)) nets))
		(bind '(& & & pids links file sid) net)
		;a network that one of these nodes added is hung from it, and would
		;be left with no way in, so it goes first. The note of a network is
		;named for the process of the node that added it
		(defq told 0)
		(each (lambda ((other & & & & its_file &))
			(if (find (str-as-num (slice its_file (const (length "/tmp/chrysalisp_"))
					(- -1 (const (length ".session"))))) pids)
				(setq told (+ told (ifn (node-stop other) 0)))))
			nets)
		;the nodes of this machine, and of the network's system if it is
		;one of its own
		(defq mbox (mail-mbox) nodes (cat (lisp-nodes :t) (if sid (lisp-nodes sid) (list)))
			doomed (list))
		(each (# (open-task (str `(mail-send (hex-decode ,(hex-encode mbox))
				(cat (str (pii-pid)) " " (hex-encode (task-nodeid)))))
			%0 +kn_call_pin 0 (mail-mbox))) nodes)
		(times (length nodes)
			(when (defq reply (mail-read-timeout mbox (task-timeout 2)))
				(bind '(pid node) (split reply " "))
				(if (find (str-as-num pid) pids) (push doomed node))))
		;each is told now, while every one of them is up to pass it on, to
		;go in a moment, by itself. One node told to tell them later may
		;be gone by then, and a node that goes cuts off those the mail
		;went through it to reach, the far side of a ring was left running
		(defq wait (/ (task-timeout 1) 2))
		(each (# (open-task (str `(progn (task-sleep ,wait) (pii-exit)))
			(hex-decode %0) +kn_call_pin 0 (mail-mbox))) doomed)
		(each (# (pii-remove (cat "/tmp/" %0))) links)
		;the note, less this network's lines
		;each line that stays ends as it did, what is noted next goes on
		;the end of the file and would run on from a last line that did not
		(defq keep :t)
		(save (apply (const cat) (cat (list "") (map (# (cat %0 (ascii-char 10))) (filter (lambda (line)
				(cond
					((starts-with "net " line) (setq keep (nql (second (split line " ")) id)))
					(keep)))
			(split (ifn (load file) "") (ascii-char 10)))))) file)
		(+ told (length doomed))))

(defun mail-read-timeout (mbox &optional timeout)
	; (mail-read-timeout mbox [timeout_us]) -> msg | :nil
	(defq timer (mail-timeout (mail-mbox) (ifn timeout (task-timeout 1)) 0))
	(when (= (mail-select (list mbox timer)) 0)
		;mail came first, so cancel the timer
		(mail-timeout timer 0 0)
		(mail-read mbox)))

(defq mail-timeout-read mail-read-timeout)

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

;jit helper
(defun jit (prefix file products)
	(bind '(& *cpu* *abi*) (map (const sym) (split (load-path) "/")))
	(defq obj_prefix (cat "obj/" *cpu* "/" *abi* "/" prefix)
		file (cat prefix file) file_age (age file))
	(when (some (# (> file_age (age (cat obj_prefix %0)))) products)
		(lock-claim-rpc obj_prefix)
		(when (some (# (> file_age (age (cat obj_prefix %0)))) products)
			(catch (within-compile-env (# (include file))) :nil))
		(lock-release-rpc obj_prefix)))
