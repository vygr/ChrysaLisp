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

(defun place-translators (plats)
	;the translators of the platforms this build makes code of, and the
	;parser they call, as their files are now, placed in a function server
	;of this worker named by their hash, so the bind of them is that and
	;not what its node bound before, from files a build has since made
	;again: they are of one layout of records. A translator only in the
	;boot image is bound as it is, it never changes
	(defq natives (list))
	(each (lambda ((cpu abi &rest _))
			(if (defq native (load (cat (load-path) (trans-name cpu abi)))) (push natives native))) plats)
	(when (nempty? natives)
		;the parser first, so a translator's link to it is to this one
		(if (defq parse (load (cat (load-path) "lib/trans/vp_parse"))) (setq natives (cat (list parse) natives)))
		(task-server (cat "trans_" (slice (hex-encode (sha256 (apply (const cat) natives))) 0 16)))
		(task-place natives)))

(defun translator (cpu abi)
	; (translator cpu abi) -> func
	;the translator to a platform, bound the first time it is asked for,
	;after they are placed
	(defq name (trans-name cpu abi) k (sym name))
	(or (get k *trans*)
		(def *trans* k (if (or (> (age (cat (load-path) name)) 0) (image-has? name)) (ffi name)
				(throw "No translator to this CPU here, make install makes them !" cpu)))))

(defun main ()
	(defq *select* (task-mboxes +_select_size) *working* :t *msg* :nil *placed* :nil *trans* (env 11))
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
							'(*kind* *unit* *abi* *cpu* *plats* *build_mode* *build_emit* *build_verb* *build_vp*)
							(first (read (string-stream (slice *msg* +_job_params -1)))))
						(unless *placed* (setq *placed* :t) (place-translators *plats*))
						;a file job is a source file, if it is to be assembled, and
						;the platforms to make code of it for, each with the hash of
						;the bundle its code is of, if that is known. Assembled, its
						;bundle of the VP of its functions goes to *build_vp*; if
						;not, the bundle there is read. Then each platform's code of
						;each function, from its text, by that platform's translator,
						;unless the bundle is the one its code is of. A bundle job is
						;a source file, its bundle and no code. An image job is a
						;platform and the functions of its boot image.
						;What was done is said: the file and the hash of its bundle
						;if it was assembled, each platform code was made for with a
						;short hash of the names of the functions and the names, an
						;image. Nothing is said of a job that goes wrong, and it has
						;no bundle written: catch any errors
						(setq *msg* (list))
						(catch (case *kind*
								(:image
									(bind '(cpu abi funcs) *unit*)
									(boot-image funcs abi cpu)
									(print "=image= " cpu " " abi))
								(:bundle
									(defq file (first *unit*)
										text (apply (const cat) (cat (list "")
											(map (# (vp-entry-text %0)) (vp-compile file)))))
									(save text (cat *build_vp* file))
									(print "=key= " file " " (hex-encode (sha256 text))))
								(:t ;what it includes is in before anything here is named,
									;it names things of its own. of is the hash of the
									;bundle if it is assembled here: not named hash, that
									;is a function, and (and) would be given it
									(unless (second *unit*) (include "lib/asm/func.inc"))
									(bind '(file assemble targets) *unit*)
									(defq of :nil text :nil funcs
										(cond
											(assemble
												(defq entries (vp-compile file) bundle (map (# (vp-entry-text %0)) entries))
												(setq text (apply (const cat) (cat (list "") bundle))
													of (hex-encode (sha256 text)))
												(map (# (list (first %0) %1)) entries bundle))
											((vp-bundle-read file *build_vp*)))
										names (map (# (str (first %0))) funcs)
										said (cat " " (slice (hex-encode (sha256 (join names " "))) 0 12) " " (join names " "))
										coded (list))
									(each (lambda ((cpu abi prev))
											(unless (and of (eql of prev))
												(defq trans (translator cpu abi))
												;giving way between functions, as the assembler does
												(each (lambda ((name text)) (vp-translate-to trans cpu abi name text) (task-slice)) funcs)
												(push coded (cat "=code= " file " " cpu " " abi said)))) targets)
									(when assemble
										(save text (cat *build_vp* file))
										(print "=key= " file " " of))
									(each (const print) coded)))
							(progn (print _) :t))
						;send reply, the key it was given, then what was said
						(mail-send *reply_mbox* (apply (const cat)
								(insert *msg* 0 (list (char *reply_key* +long_size)))))))))))
