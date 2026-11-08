;;;;;;;;;;;;;;;;;;;;
; VP Assembler Child
;;;;;;;;;;;;;;;;;;;;

;a worker with no job for this long ends, its farm has gone and did not
;say. It must be long, the farm's node may be busy, and a job sent to a
;worker that has ended is lost.
(defq +_timeout (task-timeout 60))

(enums +_select 0
	(enum main timeout))

(structure +_job 0
	(long key)
	(netid reply)
	(offset params))

;redirect print
(redefun print (&rest args)
	(push *msg* (apply (const str) (push args "\n")))
	:nil)

;debug options
(case :nil
(0 (import "lib/debug/frames.inc")))

(import "././boot/image.inc")
(import "././crypto/sha256.inc")
(ffi "sys/load/lisp_server" task-server)
(ffi "sys/load/lisp_place" task-place)

(defun fresh-translator (cpu abi)
	;the translator and the parser it calls as their files are now, placed
	;in a function server of this worker named by their hash, so the bind
	;of them is that and not what its node bound before, from files a build
	;has since made again: the two are of one layout of records. A
	;translator only in the boot image is bound as it is, it never changes
	(when (defq native (load (cat (load-path) (trans-name cpu abi))))
		;the parser first, so the translator's link to it is to this one
		(defq natives (list native) parse (load (cat (load-path) "lib/trans/vp_parse")))
		(if parse (setq natives (list parse native)))
		(task-server (cat "trans_" (slice (hex-encode (sha256 (apply (const cat) natives))) 0 16)))
		(task-place natives)))

(defun main ()
	(defq *select* (task-mboxes +_select_size) *working* :t *msg* :nil *placed* :nil)
	(within-compile-env (lambda ()
		(while *working*
			(mail-timeout (elem-get *select* +_select_timeout) +_timeout 0)
			(setq *msg* (mail-read (elem-get *select* (defq *idx* (mail-select *select*)))))
			(cond
				;timeout or quit
				((or (= *idx* +_select_timeout) (eql *msg* ""))
					(setq *working* :nil))
				;main mailbox
				((= *idx* +_select_main)
					;clear timeout
					(mail-timeout (elem-get *select* +_select_timeout) 0 0)
					;read job
					(defq *reply_key* (getf *msg* +_job_key) *reply_mbox* (getf *msg* +_job_reply))
					(each (# (def *compile_env* %0 %1))
						'(*kind* *files* *abi* *cpu* *build_mode* *build_emit* *build_verb* *build_vp*)
						(first (read (string-stream (slice *msg* +_job_params -1)))))
					(unless *placed* (setq *placed* :t) (fresh-translator *cpu* *abi*))
					;a VP job is source files, the bundle of each of the VP of its
					;functions to *build_vp*, a bundle job the same with no code of
					;any platform, a translate job is source files, their bundles to
					;this platform's code, each file stamped for this platform as it
					;is done, and catch any errors
					(setq *msg* (list))
					(catch (cond
							((find *kind* '(:vp :bundle))
								;the compile gives the VP, this keeps it: the bundle of
								;its text, and of a VP job each function translated
								;from its text as it goes, by name
								(each (lambda (file)
										(defq bundle (list))
										(each (lambda (entry)
												(push bundle (defq text (vp-entry-text entry)))
												(if (eql *kind* :vp) (vp-translate-save entry text)))
											(vp-compile file))
										(save (apply (const cat) (cat (list "") bundle)) (cat *build_vp* file))
										(if (eql *kind* :vp) (save "" (cat "obj/" *cpu* "/" *abi* "/" file))))
									*files*))
							(:t (include "lib/asm/func.inc")
								(each (# (vp-translate-file %0 *build_vp*)
									(save "" (cat "obj/" *cpu* "/" *abi* "/" %0))) *files*)))
						(progn (print _) :t))
					;send reply, the key it was given, then what was said
					(mail-send *reply_mbox* (apply (const cat)
						(insert *msg* 0 (list (char *reply_key* +long_size)))))))))))
