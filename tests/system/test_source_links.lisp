(report-header "Source Service: what a task of another system links to")
(import "service/source/app.inc")
(import "lib/crypto/sha256.inc")

(open-child "service/source/app.lisp" +kn_call_run)
(until (source-service) (task-sleep 10000))
(defq sl_svc (source-service) sl_mode (source-mode)
	sl_dir (cat "obj/vp/" sl_mode "/tests/source_lk/")
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
	(open-child (cat "(import {service/source/app.inc})" (ascii-char 10)
		"(source-import {" sid "} {" (hex-encode svc) "})" (ascii-char 10)
		"(mail-send (hex-decode {" (hex-encode mbox) "}) (str"
		" (catch (progn (ffi {" fn "} f) (f)) :failed)))") +kn_call_pin)
	(mail-read mbox))

;the node's own: f calls g, g gives 1, built into the node's place by a
;task of a system that has the same VP as the node
(save (cat (str (sl-f)) (ascii-char 10) (str (sl-g 1))) (cat sl_dir "lisp.vp"))
(when (source-caching?)
	(assert-eq "the node's f calls the node's g" "1" (sl-run "Tlk" sl_svc sl_f))

	;another system, its @Source a task here that answers as one: its f is
	;the same VP as the node's, its g its own way, it gives 2. The name is
	;the contract, what it does is that system's
	(save (cat (str (sl-f)) (ascii-char 10) (str (sl-g 2))) sl_sender)
	(defq mbox (mail-mbox))
	(open-child (cat "(import {service/source/app.inc})" (ascii-char 10)
		"(defq m {" sl_f " " (sl-hash (sl-f)) (ascii-char 10) sl_g " " (sl-hash (sl-g 2)) "}"
		" e (load {" sl_sender "}) n 2)" (ascii-char 10)
		"(mail-send (hex-decode {" (hex-encode mbox) "}) (hex-encode (task-mbox)))" (ascii-char 10)
		"(while (>= (setq n (dec n)) 0)"
		" (defq msg (mail-read (task-mbox)) p (slice msg +source_rpc_size -1))"
		" (mail-send (getf msg +source_rpc_reply_id)"
		" (cond ((starts-with {#} p) (cat {y} m)) ((starts-with {$} p) (cat {y} e)) ({n}))))")
		+kn_call_run)
	(defq sl_other (hex-decode (mail-read mbox)))
	(assert-eq "its task's f, the node's, links to its own g" "2" (sl-run "Ulk" sl_other sl_f))
	(assert-eq "and the node's f still to the node's" "1" (sl-run "Tlk" sl_svc sl_f))
	(each (# (pii-remove (cat (load-path) %0))) (list sl_f sl_g))
	(pii-remove sl_sender))
(pii-remove (cat sl_dir "lisp.vp"))

;a task of another system that sends a task of its own sends it as of
;that system, which has its files, not as of the one it runs on
(defq mbox (mail-mbox))
(open-child (cat "(import {service/source/app.inc})" (ascii-char 10)
	"(source-import {Ulk} {" (hex-encode sl_svc) "})" (ascii-char 10)
	"(mail-send (hex-decode {" (hex-encode mbox) "}) (source-task {apps/system/netmon/child.lisp}))")
	+kn_call_pin)
(assert-true "a task it sends names its originator" (found? (mail-read mbox) "{Ulk}"))
