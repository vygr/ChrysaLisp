# Syntax
## abs
### (abs num)
## add
### (add num num ...)
## age
### (age path)
## align
### (align num pow2)
## and
### (and tst ...)
## apply
### (apply lambda list)
## array
### (array [form ...])
## ascii
### (ascii str)
## bind
### (bind (param ...) seq)
## bit-and
### (bit-and num num ...)
## bit-asr
### (bit-asr num cnt)
## bit-not
### (bit-not num)
## bit-or
### (bit-or num num ...)
## bit-shl
### (bit-shl num cnt)
## bit-shr
### (bit-shr num cnt)
## bit-xor
### (bit-xor num num ...)
## button-connect-click
### (button-connect-click button id)
## canvas-blend-fbox
### (canvas-blend-fbox canvas argb x y w h)
## canvas-blend-fpoly
### (canvas-blend-fpoly canvas list argb mode)
## canvas-blend-plot
### (canvas-blend-plot canvas argb x y)
## canvas-fill
### (canvas-fill canvas argb)
## canvas-load
### (canvas-load path flags)
## canvas-set-fbox
### (canvas-set-fbox canvas argb x y w h)
## canvas-set-fpoly
### (canvas-set-fpoly canvas list argb mode)
## canvas-set-plot
### (canvas-set-plot canvas argb x y)
## canvas-swap
### (canvas-swap canvas)
## cat
### (cat seq ...)
## catch
### (catch form eform)
## char
### (char num [width])
## clear
### (clear array ...)
## cmp
### (cmp str str)
## code
### (code str)
## compose
### (compose lambda lambda)
## cond
### (cond (tst body) ...)
## copy
### (copy form)
## count-leading-ones
### (count-leading-ones num)
## count-trailing-ones
### (count-trailing-ones num)
## count-trailing-zeros
### (count-trailing-zeros num)
## cpu
### (cpu)
## create-backdrop
### (create-backdrop)
## create-button
### (create-button)
## create-canvas
### (create-canvas width height aa_scale)
## create-flow
### (create-flow)
## create-font
### (create-font name points)
## create-grid
### (create-grid)
## create-label
### (create-label)
## create-progress
### (create-progress)
## create-slave
### (create-slave)
## create-slider
### (create-slider)
## create-vdu
### (create-vdu)
## create-view
### (create-view)
## create-window
### (create-window flags)
## cubed
### (cubed num)
## curry
### (curry lambda var ...)
## debug
### (debug [form] ...)
## dec
### (dec num)
## def
### (def env var val [var val] ...)
## def?
### (def? var)
## defmacro
### (defmacro name ([arg ...]) body)
## defq
### (defq var val [var val] ...)
## defun
### (defun name ([arg ...]) body)
## div
### (div num num ...)
## divmod
### (divmod num num)
## each
### (each lambda seq ...)
## each!
### (each! start|nil end|nil lambda|nil lambda (seq ...))
## each-line
### (each-line lambda path)
## each-mergeable
### (each-mergeable lambda seq)
## each-mergeable-rev
### (each-mergeable-rev lambda seq)
## each-pipe-line
### (each-pipe-line lambda pipe)
## each-rev
### (each-rev lambda seq ...)
## elem
### (elem index seq)
## elem-set
### (elem-set index list val)
## env
### (env [num])
## eq
### (eq num num ...)
## eql
### (eql form form)
## equalp
### (equalp form form)
## eval
### (eval form [env])
## every
### (every lambda seq ...)
## fcos
### (fcos angle)
## fdiv
### (fdiv num num ...)
## ffi
### (ffi sym path flags)
## file-stream
### (file-stream path)
## filter
### (filter lambda seq)
## find
### (find elem seq)
## floor
### (floor num)
## fmod
### (fmod num num ...)
## fmul
### (fmul num num ...)
## frac
### (frac num)
## from-base-char
### (from-base-char str)
## fsin
### (fsin angle)
## fsqrt
### (fsqrt num)
## ge
### (ge num num ...)
## gensym
### (gensym)
## gt
### (gt num num ...)
## gui-add
### (gui-add view)
## if
### (if tst form [form])
## inc
### (inc num)
## insert
### (insert list str)
## insert-sym
### (insert-sym list sym)
## kernel-debug
### (kernel-debug str)
## kernel-declare
### (kernel-declare name mbox)
## kernel-total
### (kernel-total)
## lambda
### (lambda ([arg ...]) body)
## le
### (le num num ...)
## length
### (length seq)
## let
### (let [(var val) ...])
## list
### (list [form ...])
## load
### (load path)
## log2
### (log2 num)
## lst?
### (lst? form)
## lt
### (lt num num ...)
## macroexpand
### (macroexpand form)
## macroexpand-1
### (macroexpand-1 form)
## mail-declare
### (mail-declare name mbox)
## mail-enquire
### (mail-enquire name)
## mail-mymail
### (mail-mymail)
## mail-send
### (mail-send obj mbox)
## mail-trymail
### (mail-trymail)
## map
### (map lambda seq ...)
## map-rev
### (map-rev lambda seq ...)
## match?
### (match? list list)
## max
### (max num num ...)
## merge
### (merge list str)
## merge-sym
### (merge-sym list list)
## min
### (min num num ...)
## minus
### (minus num)
## mod
### (mod num num ...)
## mul
### (mul num num ...)
## ne
### (ne num num ...)
## neg
### (neg num)
## not
### (not form)
## notany
### (notany lambda seq ...)
## notevery
### (notevery lambda seq ...)
## num?
### (num? form)
## open-child
### (open-child path mode)
## open-farm
### (open-farm path num mode)
## open-pipe
### (open-pipe paths)
## open-remote
### (open-remote path cpu mode)
## opt
### (opt var val [cond])
## or
### (or tst ...)
## partition
### (partition lambda list start end)
## pipe
### (pipe str)
## pipe-read
### (pipe-read pipe)
## pipe-write
### (pipe-write pipe str)
## platform
### (platform)
## points
### (points [form ...])
## points-gen-arc
### (points-gen-arc dst stack center start end radius tol)
## points-gen-cubic
### (points-gen-cubic dst stack p1 p2 p3 p4 tol)
## points-gen-quadratic
### (points-gen-quadratic dst stack p1 p2 p3 tol)
## points-simplify
### (points-simplify dst src stack tol)
## points-stroke-polygons
### (points-stroke-polygons dst stack src join radius tol)
## points-stroke-polylines
### (points-stroke-polylines dst stack src join cap1 cap2 radius tol)
## points-transform
### (points-transform dst src m1 m2 tr)
## pop
### (pop array)
## prin
### (prin [form ...])
## print
### (print [form ...])
## progn
### (progn [form ...])
## push
### (push array form ...)
## quasi-quote
### (quasi-quote form)
## quote
### (quote form)
## range
### (range start end [step])
## rcurry
### (rcurry lambda var ...)
## read
### (read stream last_char)
## read-byte
### (read-byte index seq)
## read-char
### (read-char stream [width])
## read-cstr
### (read-cstr index seq)
## read-int
### (read-int index seq)
## read-line
### (read-line stream)
## read-long
### (read-long index seq)
## read-short
### (read-short index seq)
## reduce
### (reduce lambda seq [accum])
## reduce-rev
### (reduce-rev lambda seq [accum])
## repl
### (repl stream path)
## run
### (run path)
## save
### (save str path)
## set
### (set env var val [var val] ...)
## setd
### (setd var val [var val] ...)
## setq
### (setq var val [var val] ...)
## shuffle
### (shuffle list [start end])
## shuffled
### (shuffled list [start end])
## sign
### (sign num)
## slave-get-args
### (slave-get-args slave)
## slice
### (slice start end seq)
## slider-connect-value
### (slider-connect-value slider id)
## some
### (some lambda seq ...)
## some!
### (some! start|nil end|nil mode lambda (seq ...))
## sort
### (sort list [start end])
## sorted
### (sorted list [start end])
## split
### (split str char)
## squared
### (squared num)
## str
### (str form)
## str?
### (str? form)
## stream-available
### (stream-available stream)
## stream-write-flush
### (stream-write-flush stream)
## string-stream
### (string-stream str)
## sub
### (sub num num ...)
## swap
### (swap list index index)
## sym
### (sym form)
## sym-cat
### (sym-cat str ...)
## sym?
### (sym? form)
## task-mailbox
### (task-mailbox)
## task-sleep
### (task-sleep usec)
## throw
### (throw str form)
## time
### (time)
## times
### (times num body)
## to-base-char
### (to-base-char num)
## to-num
### (to-num str)
## trim
### (trim str [str])
## trim-end
### (trim-end str [str])
## trim-start
### (trim-start str [str])
## type?
### (type? obj)
## unless
### (unless tst body)
## until
### (until tst body)
## vdu-print
### (vdu-print vdu str)
## view-add-back
### (view-add-back view view)
## view-add-child
### (view-add-child view view)
## view-add-dirty
### (view-add-dirty view x y w h)
## view-add-front
### (view-add-front view view)
## view-add-opaque
### (view-add-opaque view x y w h)
## view-change
### (view-change view x y w h)
## view-dirty
### (view-dirty view)
## view-dirty-all
### (view-dirty-all view)
## view-event
### (view-event view str)
## view-find-id
### (view-find-id view id)
## view-get-bounds
### (view-get-bounds view)
## view-layout
### (view-layout view)
## view-opaque
### (view-opaque view)
## view-pref-size
### (view-pref-size view)
## view-set-bounds
### (view-set-bounds view x y w h)
## view-sub
### (view-sub view)
## view-sub-opaque
### (view-sub-opaque view x y w h)
## when
### (when tst body)
## while
### (while tst body)
## window-connect-close
### (window-connect-close window id)
## window-connect-layout
### (window-connect-layout window id)
## window-connect-max
### (window-connect-max window id)
## window-connect-min
### (window-connect-min window id)
## window-set-status
### (window-set-status window str)
## window-set-title
### (window-set-title window str)
## within-compile-env
### (within-compile-env lambda)
## write
### (write stream str)
## write-char
### (write-char stream list|num [width])
## write-line
### (write-line stream str)
