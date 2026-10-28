(report-header "Source Service: a task's files read from the system that sent it")
(import "service/source/app.inc")
(import "apps/system/netmon/app.inc")
(import "lib/crypto/sha256.inc")

;one to a system, which only its own tasks look up: what it is is given
;to a task that is sent, it is said to nobody else
;one. Tests at once may each start one where a session has none up, as
;a rack one has no terminal to: two started together give way to the
;lowest, (mail-single), within a second, so this waits for that. The
;session's own may come up later still, and be the lowest, so the one
;there is now is looked up each time one is written into a task or a
;request, as any task does; an ask finds it again by itself
(open-child "service/source/app.lisp" +kn_call_run)
(until (= 1 (length (mail-enquire "@Source,"))) (task-sleep 10000))
(defq so_svc (source-service))
(assert-true "it is up, as @Source" so_svc)
(assert-eq "and is not said to the mesh" 0 (length (mail-enquire "*Source,")))

;it gives the Lisp of the tree, and nothing else of it: the keys are in
;the tree, mesh_key and keys/, and a user's own folder
(defq so_inc "apps/system/netmon/app.inc")
;what it gives is waited for: a service busy for other machines, as when
;three are tested at once, is slow, and is not wrong
(assert-eq "it gives a file that is imported, as it is" (load so_inc) (source-ask so_svc so_inc :t))
(assert-eq "and usr/env.inc, which is the system's" (load "usr/env.inc") (source-ask so_svc "usr/env.inc" :t))
(assert-list-eq "not the key, the keyring, a user's own, out of the tree, or one not there"
	'(:nil :nil :nil :nil :nil :nil)
	(map (# (source-ask so_svc %0 :t))
		'("mesh_key" "keys/rig" "usr/Test/env.inc" "apps/../usr/Test/env.inc" "/etc/hosts" "no/such.lisp")))

;a stream of a file of a system: of the file on this one, asked of the
;service where the system is another, the same either way
(defq so_here (source-stream (hex-encode (system-id)) (source-service) so_inc)
	so_there (source-stream "0" (source-service) so_inc))
(assert-eq "a file of this system is read as a file" (load so_inc) (read-blk so_here 100000))
(assert-eq "of another, asked of its service, the same" (load so_inc) (read-blk so_there 100000))
(assert-eq "and none if it will not give it" :nil (source-stream "0" (source-service) "mesh_key"))

;a system at the same waypoint as this one: what it has this one has, a
;file of it is read here and nothing is asked of a @Source, not even of
;none; and still not one it would not give
(defq *source_same* :t)
(assert-eq "at the same waypoint, a file is read here, nothing asked" (load so_inc) (read-blk (source-stream "0" :nil so_inc) 100000))
(assert-eq "and none it would not give" :nil (source-stream "0" :nil "mesh_key"))
(undef (env) '*source_same*)

;the waypoint this system's server zero is of: none, or the one its tree is
;noted at, where make install built and no make has built since
(defq so_wp (source-waypoint))
(assert-true "server zero's waypoint, none or the tree's"
	(or (not so_wp) (eql so_wp (trim (first (split (load "waypoint") "\n"))))))

;NetMon's child, sent as what source-task makes, replies; and again with
;the system said to be another, so that each import, the child's own of
;./app.inc too, is asked of the service as on another machine
(defq so_task (source-task "apps/system/netmon/child.lisp"))
(assert-true "a task to send is Lisp to run, not a path" (starts-with "(" so_task))
(assert-true "and says the waypoint this system is at" (found? so_task (cat "{" (ifn so_wp "") "})")))
(defun so-child (task)
	(defq id (open-child task +kn_call_pin) mbox (mail-mbox))
	(mail-send id mbox)
	(defq msg (mail-read mbox))
	(mail-send id "")
	(getf msg +reply_task_count))
(assert-true "it runs, and replies" (> (so-child so_task) 0))
(assert-true "and from another system, its imports asked of the service"
	(> (so-child (cat "(import {service/source/app.inc})" "\n"
		"(source-import {0} {" (hex-encode (source-service)) "})" "\n"
		"(import {apps/system/netmon/child.lisp})")) 0))

;and from another system at the same waypoint as this one, its @Source a
;mailbox nobody reads: it runs, as nothing is asked of it. Only where make
;install built at the waypoint the tree is at, and no make has since
(if so_wp
	(assert-true "from a system at the same waypoint, nothing asked"
		(> (so-child (cat "(import {service/source/app.inc})" "\n"
			"(source-import {0} {" (hex-encode (mail-mbox)) "} {" so_wp "})" "\n"
			"(import {apps/system/netmon/child.lisp})")) 0))
	(test-skip "from a system at the same waypoint" "this system's build is not of a waypoint, make install at one"))

;natives of another system: the hash of each asked of the service, and
;the VP of those not the same as the node's own. Here the other system is
;this one, so the VP is the node's own, server zero's: one the node has not
;built is translated into the node's own place if it may be kept. The VP is
;NetSpeed's vops under names of this run's own
(defq so_pid "run" so_sid (cat "T" so_pid)
	so_dir (cat "obj/vp/tests/source_" so_pid "/")
	so_name (cat "tests/source_" so_pid "/vops"))
(defq so_entry (first (read (file-stream "obj/vp/apps/system/netspeed/lisp.vp")))
	so_old (first so_entry))
(save (str (cat (list so_name (map (lambda (inst)
		(if (list? inst) (map (# (if (eql %0 so_old) so_name %0)) inst) inst)) (second so_entry)))
	(slice so_entry 2 -1))) (cat so_dir "lisp.vp"))
(defun so-native (fn)
	;what a task of the other system has of its servers, and of the native
	(defq mbox (mail-mbox))
	(open-child (cat "(import {service/source/app.inc})" "\n"
		"(source-import {" so_sid "} {" (hex-encode (source-service)) "})" "\n"
		"(mail-send (hex-decode {" (hex-encode mbox) "}) (str (list (task-server)"
		" (catch (progn (ffi {" fn "} f) (length (f))) :failed))))") +kn_call_pin)
	(mail-read mbox))
(when (source-caching?)
	(assert-eq "a native of VP the node has, not built, translated into the node's place, and run"
		(str (list (list so_sid) 3)) (so-native so_name))
	(assert-true "it is the node's own now" (/= (age (cat (load-path) so_name)) 0))

	;natives placed in a task's server, in memory, then bound: the same
	;native from the server, and one with a link to a path there is none
	;of, which is not placed, the server as it was
	(defq so_native (load (cat (load-path) so_name)) so_bad so_native
		lo (get-ushort so_native +fn_header_links) po (get-ushort so_native +fn_header_paths))
	(while (/= lo po)
		(defq at (+ (get-long so_native lo) lo))
		(if (eql (get-cstr so_native at) "sys/pii/time")
			(setq so_bad (cat (slice so_native 0 (+ at 11)) "f" (slice so_native (+ at 12) -1))))
		(setq lo (+ lo +ptr_size)))
	(task-server (cat "P" so_pid))
	(assert-eq "a native placed in the task's server" :t (task-place (list so_native)))
	(assert-eq "and bound from there, and run" 3 (length (progn (ffi so_name so_f) (so_f))))
	(assert-eq "one with a link to nothing is not placed" :nil (task-place (list so_bad)))
	(assert-list-eq "the server is the task's" (list (cat "P" so_pid)) (filter (# (eql %0 (cat "P" so_pid))) (task-server)))
	(task-server "")
	;a task of a server that ends lets the rest go too
	(so-native so_name)
	(until (empty? (task-server)) (task-sleep 10000))
	(assert-list-eq "and the servers go when no task has them" '() (task-server))
	(pii-remove (cat (load-path) so_name)))
(pii-remove (cat so_dir "lisp.vp"))

;a task of another system linked as that system: the node's functions that
;call one of its own, all the way down, linked again for its tasks
(defq sl_dir "obj/vp/tests/source_lk/"
	sl_f "tests/source_lk/f" sl_g "tests/source_lk/g"
	sl_sender (cat sl_dir "sender.txt"))

(defun sl-f ()
	;VP of a Lisp function that returns what g gives
	(first (read (string-stream (str `(,sl_f (progn
		(emit-label '_0) (emit-long -1) (emit-short _9 _2 _5 _6 256)
		(emit-label '_1) (emit-string ,sl_f) (emit-byte 0)
		(emit-byte (- _2 _1)) (emit-align 8 (- _2 _1))
		(emit-label '_2)
		(emit-alloc 16) (emit-cpy-ri :r0 :rsp 0) (emit-call-p _5)
		(emit-cpy-rr :r0 :r1) (emit-cpy-ir :rsp 0 :r0) (emit-free 16) (emit-ret)
		(emit-align 8) (emit-label '_5) (emit-long (- _6 *pc*))
		(emit-label '_6) (emit-string ,sl_g) (emit-byte 0)
		(emit-align 8) (emit-label '_9))))))))

(defun sl-g (n)
	;VP of a function that gives the number n
	(first (read (string-stream (str `(,sl_g (progn
		(emit-label '_0) (emit-long -1) (emit-short _9 _2 _5 _6 256)
		(emit-label '_1) (emit-string ,sl_g) (emit-byte 0)
		(emit-byte (- _2 _1)) (emit-align 8 (- _2 _1))
		(emit-label '_2)
		(emit-cpy-cr ,n :r0) (emit-jmp-p _5)
		(emit-align 8) (emit-label '_5) (emit-long (- _6 *pc*))
		(emit-label '_6) (emit-string "class/num/create") (emit-byte 0)
		(emit-align 8) (emit-label '_9))))))))

(defun sl-hash (entry) (hex-encode (sha256 (str entry))))

(defun sl-run (sid svc fn)
	;what a task of the system sid, its @Source svc, gets from (fn)
	(defq mbox (mail-mbox))
	(open-child (cat "(import {service/source/app.inc})" "\n"
		"(source-import {" sid "} {" (hex-encode svc) "})" "\n"
		"(mail-send (hex-decode {" (hex-encode mbox) "}) (str"
		" (catch (progn (ffi {" fn "} f) (f)) :failed)))") +kn_call_pin)
	(mail-read mbox))

;the node's own: f calls g, g gives 1, built into the node's place by a
;task of a system that has the same VP as the node
(save (cat (str (sl-f)) "\n" (str (sl-g 1))) (cat sl_dir "lisp.vp"))
(when (source-caching?)
	(assert-eq "the node's f calls the node's g" "1" (sl-run "Tlk" (source-service) sl_f))

	;another system, its @Source a task here that answers as one: its f is
	;the same VP as the node's, its g its own way, it gives 2. The name is
	;the contract, what it does is that system's
	(save (cat (str (sl-f)) "\n" (str (sl-g 2))) sl_sender)
	(defq mbox (mail-mbox))
	(open-child (cat "(import {service/source/app.inc})" "\n"
		"(defq m {" sl_f " " (sl-hash (sl-f)) "\n" sl_g " " (sl-hash (sl-g 2)) "}"
		" e (load {" sl_sender "}) n 2)" "\n"
		"(mail-send (hex-decode {" (hex-encode mbox) "}) (hex-encode (task-mbox)))" "\n"
		"(while (>= (setq n (dec n)) 0)"
		" (defq msg (mail-read (task-mbox)) p (slice msg +source_rpc_size -1))"
		" (mail-send (getf msg +source_rpc_reply_id)"
		" (cond ((starts-with {#} p) (cat {y} m)) ((starts-with {$} p) (cat {y} e)) ({n}))))")
		+kn_call_run)
	(defq sl_other (hex-decode (mail-read mbox)))
	(assert-eq "its task's f, the node's, links to its own g" "2" (sl-run "Ulk" sl_other sl_f))
	(assert-eq "and the node's f still to the node's" "1" (sl-run "Tlk" (source-service) sl_f))
	(each (# (pii-remove (cat (load-path) %0))) (list sl_f sl_g))
	(pii-remove sl_sender))
(pii-remove (cat sl_dir "lisp.vp"))

;a task of another system that sends a task of its own sends it as of
;that system, which has its files, not as of the one it runs on
(defq mbox (mail-mbox))
(open-child (cat "(import {service/source/app.inc})" "\n"
	"(source-import {Ulk} {" (hex-encode (source-service)) "})" "\n"
	"(mail-send (hex-decode {" (hex-encode mbox) "}) (source-task {apps/system/netmon/child.lisp}))")
	+kn_call_pin)
(assert-true "a task it sends names its originator" (found? (mail-read mbox) "{Ulk}"))

;a task of another system that imports a module, and all it imports: the
;@Source here is asked once for the whole of it, and asks that system once,
;the hash of each file, and its copies here are the same, so no text. A
;second task like it is answered from what was kept, that system not asked
;each @Source counts what it does, and more than one can be up for a
;while, one started since being the one there is now: a step's counts are
;those of the @Source it asked, and one that asked another, that came up
;meanwhile, is done again
(defun so-stats (svc)
	; (so-stats svc) -> (asked kept told sought), of that @Source
	(first (read (string-stream (source-ask svc "!")))))
(defun so-counted (fn)
	; (so-counted fn) -> (was what now)
	;(fn svc) -> (what asked), asked the @Source it asked, :nil if not svc
	(defq out :nil)
	(until out
		(defq svc (source-service) was (so-stats svc) got (fn svc))
		(if (eql (second got) svc) (setq out (list was (first got) (so-stats svc)))))
	out)
(defun so-tree-task (svc)
	;what a task of the other system was given, as it imports Mandelbrot's
	;app.inc and so all that imports, and the @Source here it asked
	(defq mbox (mail-mbox))
	(open-child (cat "(import {service/source/app.inc})" "\n"
		"(source-import {0} {" (hex-encode svc) "})" "\n"
		"(import {apps/science/mandelbrot/app.inc})" "\n"
		"(mail-send (hex-decode {" (hex-encode mbox) "}) (str (list (length (tolist *source_got*)) (hex-encode (source-here)))))")
		+kn_call_pin)
	(bind '(got asked) (first (read (string-stream (mail-read mbox)))))
	(list got (hex-decode asked)))
;two tasks of it, the counts between them too
(bind '(so_was (so_got so_mid so_got2) so_now) (so-counted (lambda (svc)
	(bind '(got1 asked1) (so-tree-task svc))
	(defq mid (so-stats svc))
	(bind '(got2 asked2) (so-tree-task svc))
	(list (list got1 mid got2) (and (eql asked1 svc) (eql asked2 svc) svc)))))
(assert-true "a task of another system is given the module and what it imports" (> so_got 1))
(assert-eq "in one ask of the @Source here" 1 (- (first so_mid) (first so_was)))
(assert-eq "which asks that system once" 1 (- (third so_mid) (third so_was)))
(assert-eq "a second task asks once too" 1 (- (first so_now) (first so_mid)))
(assert-eq "and is answered from what was kept" 1 (- (second so_now) (second so_mid)))
(assert-eq "that system is not asked again" 0 (- (third so_now) (third so_mid)))

;a task sent with a @Source that has gone since: one started again has
;another mailbox. Not answered, the @Source here asks a kernel of that
;system which it has now, and asks that one. A mailbox nobody reads is the
;one that has gone
(bind '(so_was so_gone so_now) (so-counted (lambda (svc)
	(list (source-ask svc (cat "%" (hex-encode (system-id)) " " (hex-encode (mail-mbox)) " " so_inc) (* 3 +source_wait))
		(if (eql (source-service) svc) svc)))))
(assert-eq "a file asked of a @Source that has gone comes from the one there is now"
	so_inc (first (first (source-unpack (ifn so_gone "")))))
(assert-eq "found by asking a kernel of that system" 1 (- (elem-get so_now 3) (elem-get so_was 3)))

;started twice at once, by two that each found none: every copy answers,
;one that gives way too, till nothing comes to it, and one of them stays,
;the lowest, which every task picks, (mail-single). Each is asked by the
;mailbox it was started with: one can give way before it is seen listed
(defq so_all (list (open-child "service/source/app_impl.lisp" +kn_call_run)
	(open-child "service/source/app_impl.lisp" +kn_call_run)))
(assert-true "each copy answers, the one that gives way too"
	(every (# (eql (load so_inc) (source-ask %0 so_inc :t))) so_all))
(until (= (length (mail-enquire "@Source,")) 1) (task-sleep 10000))
(assert-eq "and one stays" 1 (length (mail-enquire "@Source,")))
(assert-eq "which answers" (load so_inc) (source-ask (source-service) so_inc :t))

;a @Source of this system that has gone, a mailbox of it nobody reads: an
;ask that hears nothing for +source_wait looks for the one there is now,
;and asks it
(assert-eq "an ask of one of this system that has gone is answered by the one there is"
	(load so_inc) (source-ask (mail-mbox) so_inc (* 3 +source_wait)))
