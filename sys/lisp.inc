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

(defun node-spawn (&optional num)
	; (node-spawn [num]) -> (pid ...)
	;start num more nodes on this machine, default 1, each linked to this
	;node and to each other. A pid of -1 is a node the host could not start.
	(setd num 1)
	(if (< num 1) (list)
	(progn (defq links (map (lambda (_) (list)) (range 0 (inc num)))
		chars "0123456789abcdefghijklmnopqrstuvwxyz")
	(each (lambda (i) (each (lambda (j)
			;a link name is ???-???, the form the stop scripts clear away
			(defq name (apply (const cat) (map (lambda (_)
				(if (= _ 3) "-" (elem-get chars (random 36)))) (range 0 7))))
			(push (elem-get links i) name)
			(push (elem-get links j) name))
		(range (inc i) (inc num)))) (range 0 num))
	(defq pids (map (lambda (names)
		(pii-spawn (join (map (lambda (name) (cat "-l " name)) names) " ")))
		(rest links)))
	(each (const node-link) (first links))
	;leave the pids and the link names for the launch script that started
	;this node's session, it stops what its nodes started, see funcs.sh
	(unless (eql (os) 'Windows)
		(defq file (cat "/tmp/chrysalisp_" (str (pii-pid)) ".session"))
		(save (cat (if (/= (age file) 0) (load file) "")
			(join (map (# (cat "pid " (str %0) (ascii-char 10))) (filter (# (> %0 0)) pids)) "")
			(join (map (# (cat "link " %0 (ascii-char 10))) (unique (flatten links))) "")) file))
	pids)))

(defun node-auto (&optional per_cpu)
	; (node-auto [per_cpu]) -> (pid ...)
	;size the network to this machine. Start nodes till there are per_cpu
	;for each processor, default 1, and no more than 32, then wait for
	;them to be seen, so what is started next can spread over them. One
	;for each is where a full build is quickest, more only share them out.
	(setd per_cpu 1)
	(defq before (length (lisp-nodes))
		pids (node-spawn (max 0 (- (min 32 (* per_cpu (pii-cpus))) before)))
		want (+ before (length (filter (lambda (pid) (> pid 0)) pids)))
		t0 (pii-time))
	(while (and (< (length (lisp-nodes)) want) (< (- (pii-time) t0) (task-timeout 10)))
		(task-sleep 10000))
	pids)

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
