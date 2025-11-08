;;;;;;;;;;;
; Debugging
;;;;;;;;;;;

;module
(env-push)

(defq +debug_width 60 +LF (ascii-char 10))

(structure +debug 0
	(netid reply origin)
	(int type)
	(offset data))

(defun debug-service ()
	(when (and (not (eql *debug_state* :gui))
			(/= 0 (length (defq services (mail-enquire "*Debug")))))
		(to-net-id (second (split (pop services) ",")))))

(defun debug-ipc (service type s)
	(mail-send service (setf-> (cat (str-alloc +debug_size) s)
		(+debug_reply (defq mbox (mail-mbox)))
		(+debug_origin (task-mbox))
		(+debug_type type)))
	(setq *debug_state* (mail-read mbox)))

(defun debug-trunc (s w &optional d)
	(if (<= (length s) w) s
		(if d
			(cat "... " (slice s (- 3 w) -1))
			(cat (slice s 0 (- w 4)) " ..."))))

(defun debug-sanitize (s w &optional d)
	(if (eql s "") s
		(apply (const cat) (map
			(# (if (<= 0x20 (code %0) 0x7e)
				(if (find %0 "{}") "." %0) "."))
			(debug-trunc s w d)))))

(defun debug-info (rval sform e n)
	(defq e (tolist e) max_var_size 0
		msg (cap (+ (* (length e) 4) 16) (list))
		u (pad "" (+ (length n) 4) ";;;;;;;;;;;;;;;;;;;;;;;;"))
	(push msg u +LF
		"; " n " ;" +LF
		u +LF +LF
		(debug-sanitize sform +debug_width) +LF +LF
		(debug-sanitize (str rval) +debug_width) +LF +LF)
	(each (lambda ((var val))
		(setq max_var_size (max (length var) max_var_size))) e)
	(each (lambda ((var val))
		(setq var (pad var max_var_size)
			val (debug-sanitize (str val) (- (const (- +debug_width 3)) max_var_size) :t))
		(push msg var " : " val +LF)) e)
	(apply (const cat) (push msg +LF)))

(defun debug-msg (rval sform e n)
	(when (defq service (debug-service))
		(debug-ipc service 0 (if (eql *debug_state* ":forward")
			"" (debug-info rval sform e n))))
	rval)

(defun debug-brk-func (break_info condition)
	(and condition (defq service (debug-service))
		(debug-ipc service 1 (debug-info :nil :nil (penv) (cat "<break> " break_info)))))

(defun debug-form? (form)
	(cond
		((not (list? form))
			;not a list so not interested
			:nil)
		((list? (defq func (first form)))
			;a list so just step in
			0)
		((not (sym? func))
			;not a symbol so not interested
			:nil)
		((some (# (starts-with %0 func))
			'(quote const exec macro quasi- static- debug- profile- stack-))
			;blacklisted function, do not step in or wrap !
			:nil)
		((find func
			;whitelisted
			;built in ffi
			'(
			! % * + - . / /= < << <= = > >= >> >>> abs apply array bfind bind bskip bskipn
			canvas-from-argb32 canvas-to-argb32 cap cat catch char clear cmp code cond
			condn copy cos create-font create-stdio def def? defq dim dim-get dim-set each!
			elem-get elem-set env env-pop env-push env-resize eql eval eval-list expand ffi
			file-stream filter! find first fixeds fixeds-floor fixeds-frac floor
			font-glyph-bounds font-glyph-paths font-glyph-ranges font-info font-sym-texture
			frac gensym get get-field gui-deinit gui-event gui-info gui-init gui-update
			hash identity if ifn in-next-msg in-stream io-stream kernel-stats last length
			lines! list lmatch? load load-path logand logior logxor macroexpand
			mail-declare mail-enquire mail-forget mail-mbox mail-nodes mail-poll mail-read
			mail-select mail-send mail-timeout mail-validate map! max memory-stream merge
			min most n2f n2i n2r neg num-intern nums nums-abs nums-add nums-div nums-dot
			nums-max nums-min nums-mod nums-mul nums-scale nums-sub nums-sum obj-ref
			out-stream partition path path-filter path-gen-arc path-gen-cubic
			path-gen-quadratic path-simplify path-stroke-polygon path-stroke-polyline
			path-svg path-transform penv pii-dirlist pii-fstat pii-read-char pii-remove
			pii-time pii-write-char pivot pop prebind prin print progn push random rbskip
			rbskipn read read-avail read-bits read-char read-line reals recip reduce! repl
			repl-info rest rfind save second set set-field setq sign sin slice some! sqrt
			str str-alloc str-to-num stream-avail stream-flush stream-seek string-stream
			sym task-count task-mbox task-sleep task-slice third throw tolist type-of undef
			until weak-ref while write-bits write-char write-line
			;functions
			Mat3x2-rotz-f Mat3x2-skewx-f Mat3x2-skewy-f Mat3x3-rotx Mat3x3-roty Mat3x3-rotz
			Mat3x3-scale Mat3x3-unity Mat4x4-frustum Mat4x4-rotx Mat4x4-roty Mat4x4-rotz
			Mat4x4-scale Mat4x4-translate Mat4x4-unity Normal SVG-Canvas SVG-info XML-parse
			abi abi-args abi-call-table abs-path age align array-bind-args array-get-args
			array? ascii-lower ascii-upper assign-fields assign-net-id assign-node-id atom?
			audio-add-rpc audio-change-rpc audio-play-rpc audio-remove-rpc bit-mask
			bounding-box bounding-sphere build-brackets build-syntax built-in
			byte-to-hex-str cache char-class char-to-num cheby-term check-date circle
			clip-get-rpc clip-put-rpc close-apps compile concat? cook-key cpp-node? cpu
			create date decode-date deferror defset defsetq deg-to-rad destroy dispatch
			dispatch-job each-mergeable empty? encode-date ends-with env? erase erase-line
			escape even? exec export export-classes export-symbols files-all
			files-all-depends files-depends files-dirs firstelem firstelem-no-trace fixed?
			fixeds? flatten float-time flush-bits found? func? gather gen-norms get-by-val
			get-cstr get-date get-year get-yeardays goal-distance gui-rpc
			huffman-build-freq-map huffman-compress huffman-compress-static
			huffman-decompress huffman-decompress-static huffman-read-codebook
			huffman-write-codebook id-decode id-encode in-get-state in-mbox in-set-state
			insert insert-line insert-line-wrapped insertelem insertelem-no-trace
			int-to-hex-str iron is-text-char? iso-surface join lambda-func? lambda?
			last-line leapyears-since-epoch lighting lighting-at3 lisp-elem-index
			lisp-node? lisp-nodes list-bind-args list-cast-args list? lists load-fields
			load-net-id load-node-id load-stream lock-claim-rpc lock-release-rpc log2
			lognot long-to-hex-str macro-func? macro? mat3x2-mul-f mat3x3-mul
			mat3x3-vec3-mul mat4x4-invert mat4x4-mul mat4x4-vec3-mul mat4x4-vec4-mul match
			match? matches max-length min-length mouse-type neg? nempty? next-utf8 nil? nlo
			nlz nto ntz num-to-char num-to-utf8 num? nums? odd? open-child open-pipe
			open-remote open-task opt-flag opt-mesh opt-num opt-str opt-vectors options
			options-find options-print options-split os output output-c output-state
			output-token pad path-fill-and-stroke pipe-farm pipe-run pipe-split pos?
			postfix pow print-mat push-attributes push-line quasi-quote? query quote? range
			read-attribute read-col read-data read-rgb read-transform real-norm real-pack
			real-unpack real? reals? redo render-object-tris render-object-verts replace
			restart rle-compress rle-decompress rotate s2i save-fields save-net-id
			save-node-id scatter search seq? set-focus-id set-line set-mouse-id
			short-to-hex-str shuffle sort split start starts-with stdio-get-args stop
			str-as-num str? stream-diff stream-patch substr swap sym? task-mboxes
			task-nodeid task-timeout time-in-seconds to-array to-lower to-net-id
			to-service-id to-upper tokenize tree-buckets tree-collection? tree-decode
			tree-encode tree-load tree-node tree-save tree-type trim trim-end trim-start
			type-to-size undo unescape unique unzip update-mods-down update-mods-up url-ctx
			url-ext url-ext-index url-list url-split usort vec-abs vec-add vec-asr
			vec-chebyshev-distance vec-clr vec-collide-lines-2d vec-collide-thick-lines-2d
			vec-cpy vec-cross-3d vec-det vec-dif vec-dist vec-dist-to-line vec-div vec-dup
			vec-euclidean-distance vec-fdet vec-fdistance-squared vec-fdiv vec-fdot
			vec-flength vec-flength-squared vec-floor vec-fmod vec-fmul vec-fnorm vec-frac
			vec-frecip vec-fscale vec-fsquare vec-goto vec-high vec-intersect-2d
			vec-intersect-lines-2d vec-length vec-length-squared vec-load vec-load-int
			vec-load-long vec-loop-until vec-loop-while vec-loop-while-endswitch vec-low
			vec-manhattan vec-manhattan-distance vec-max vec-min vec-mod vec-mul vec-norm
			vec-over vec-perp vec-perp-2d vec-pop vec-push vec-reflect vec-scale vec-sdist
			vec-sdist-to-line vec-set vec-shl vec-shr vec-square
			vec-squared-euclidean-distance vec-store vec-store-int vec-store-long vec-sub
			vec-sum vec-tee vec-tmp vec-top vertex-interp write-line-lf write-token zip
			))
			;wrap this form
			1)
		(:t ;step in but do not wrap
			0)))

(defun debug-instrument (name form)
	(defq stack (list form))
	(while (defq l (pop stack))
		(each (lambda (e)
			;valid form ?
			(when (defq m (debug-form? e))
				;wrap the form ?
				(when (> m 0) (elem-set l (!)
					(static-qq (debug-msg ,e ,(str e) (env) ,name))))
				;step into form
				(push stack e))) l)) form)

(redefmacro debug-brk (brk_id &optional condition)
	(setd condition :t)
	(bind '(file line) (repl-info))
	(defq info (str "|" brk_id "|" condition "|" file "|" line "|"))
	(static-qq (debug-brk-func ,info ,condition)))

(redefmacro defun (n a &rest _)
	(static-qq (defq ,n (,'lambda ,a ~(debug-instrument (str n) _)))))

(redefmacro defmethod (n a &rest _)
	(static-qq (def (def? :vtable this) ,n (,'lambda (this ~a)
		~(debug-instrument (cat *class* " " n) _)))))

;module
(export-symbols
	'(defun defmethod debug-brk))
(env-pop)
