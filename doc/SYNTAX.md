# Syntax
## abi
### (abi) -> sym
## abs
### (abs num) -> num
## add
### (add num num ...)
## age
### (age path)
## align
### (align num pow2) -> num
## and
### (and tst ...)
## apply
### (apply lambda list)
## array
### (array [form ...])
## ascii
### (ascii str) -> num
## bind
### (bind (param ...) seq)
## bit-and
### (bit-and num num ...)
## bit-asr
### (bit-asr num cnt)
## bit-not
### (bit-not num) -> num
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
## canvas-fbox
### (canvas-fbox canvas x y w h)
## canvas-fill
### (canvas-fill canvas argb)
## canvas-fpoly
### (canvas-fpoly canvas x y mode list)
## canvas-load
### (canvas-load path flags)
## canvas-next-frame
### (canvas-next-frame canvas)
## canvas-plot
### (canvas-plot canvas x y)
## canvas-set-color
### (canvas-set-color canvas argb)
## canvas-set-flags
### (canvas-set-flags canvas flags)
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
### (code str [width index])
## compose
### (compose lambda lambda) -> lambda
## cond
### (cond (tst body) ...)
## const
### (const form)
## copy
### (copy form)
## count-leading-ones
### (count-leading-ones num) -> num
## count-trailing-ones
### (count-trailing-ones num) -> num
## count-trailing-zeros
### (count-trailing-zeros num) -> num
## cpu
### (cpu) -> sym
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
## create-scroll
### (create-scroll flags)
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
### (cubed num) -> num
## curry
### (curry lambda var ...) -> lambda
## debug
### (debug [form ...])
## dec
### (dec num) -> num
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
### (divmod num num) -> list
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
### (equalp form form) -> bool
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
### (filter lambda seq) -> seq
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
### (from-base-char str) -> num
## fsin
### (fsin angle)
## fsqrt
### (fsqrt num)
## ge
### (ge num num ...)
## gensym
### (gensym)
## get-byte
### (get-byte str index) -> num
## get-cstr
### (get-cstr str index) -> str
## get-int
### (get-int str index) -> num
## get-long
### (get-long str index) -> num
## get-short
### (get-short str index) -> num
## gt
### (gt num num ...)
## gui-add
### (gui-add view)
## if
### (if tst form [form])
## import
### (import path)
## inc
### (inc num) -> num
## insert
### (insert list str) -> list
## insert-sym
### (insert-sym list sym) -> list
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
### (log2 num) -> num
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
### (map lambda seq ...) -> list
## map-rev
### (map-rev lambda seq ...) -> list
## match?
### (match? list list)
## max
### (max num num ...)
## merge
### (merge list str) -> list
## merge-sym
### (merge-sym list1 list2) -> list1
## min
### (min num num ...)
## minus
### (minus num) -> num
## mod
### (mod num num ...)
## mul
### (mul num num ...)
## ne
### (ne num num ...)
## neg
### (neg num) -> num
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
## pad
### (pad form width [str]) -> str
## partition
### (partition lambda list start end)
## pipe
### (pipe str)
## pipe-read
### (pipe-read pipe)
## pipe-write
### (pipe-write pipe str)
## points
### (points [form ...])
## points-abs
### (points-abs points [points])
## points-add
### (points-add points points [points])
## points-div
### (points-div points points [points])
## points-fdiv
### (points-fdiv points points [points])
## points-fmod
### (points-fmod points points [points])
## points-fmul
### (points-fmul points points [points])
## points-frac
### (points-frac points [points])
## points-fscale
### (points-fscale points scale [points])
## points-gen-arc
### (points-gen-arc stack cx cy start end radius tol dst) ->
## points-gen-cubic
### (points-gen-cubic stack p1x p1y p2x p2y p3x p3y p4x p4y tol dst) -> dst
## points-gen-quadratic
### (points-gen-quadratic stack p1x p1y p2x p2y p3x p3y tol dst) -> dst
## points-mod
### (points-mod points points [points])
## points-mul
### (points-mul points points [points])
## points-simplify
### (points-simplify dst src stack tol)
## points-stroke-polygons
### (points-stroke-polygons stack radius tol join src dst) -> dst
## points-stroke-polylines
### (points-stroke-polylines stack radius tol join cap1 cap2 src dst) -> dst
## points-sub
### (points-sub points points [points])
## points-sum
### (points-sum points)
## points-transform
### (points-transform dst src m1x m1y m2x m2y trx try)
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
### (range start end [step]) -> list
## rcurry
### (rcurry lambda var ...) -> lambda
## read
### (read stream last_char)
## read-char
### (read-char stream [width])
## read-line
### (read-line stream)
## reduce
### (reduce lambda seq [accum])
## reduce-rev
### (reduce-rev lambda seq [accum])
## repl
### (repl stream path)
## save
### (save str path)
## set
### (set env var val [var val] ...)
## setd
### (setd var val [var val] ...)
## setq
### (setq var val [var val] ...)
## shuffle
### (shuffle list [start end]) -> list
## shuffled
### (shuffled list [start end]) -> list
## sign
### (sign num) -> num
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
### (sort list [start end]) -> list
## sorted
### (sorted list [start end]) -> list
## split
### (split str char)
## squared
### (squared num) -> num
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
### (sym-cat str ...) -> sym
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
### (to-base-char num) -> str
## to-num
### (to-num str) -> num
## trim
### (trim str [str]) -> str
## trim-end
### (trim-end str [str]) -> str
## trim-start
### (trim-start str [str]) -> str
## type?
### (type? obj)
## undef
### (undef env var [var] ...)
## unless
### (unless tst body)
## until
### (until tst body)
## vdu-print
### (vdu-print vdu str)
## view-add-back
### (view-add-back parent child)
## view-add-child
### (view-add-child parent child)
## view-add-dirty
### (view-add-dirty view x y w h)
## view-add-front
### (view-add-front parent child)
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
### (write-line stream str) -> str
