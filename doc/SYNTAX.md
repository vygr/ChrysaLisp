# Syntax
## add
### (add num num ...)
## age
### (age path)
## apply
### (apply func args)
## bind
### (bind params args)
## bind
### (bind vars vals)
## bit-and
### (bit-and num num ...)
## bit-asr
### (bit-asr num cnt)
## bit-or
### (bit-or num num ...)
## bit-shl
### (bit-shl num cnt)
## bit-shr
### (bit-shr num cnt)
## bit-xor
### (bit-xor num num ...)
## call
### (call slot obj [param ...])
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
## cond
### (cond (tst form ...) ...)
## copy
### (copy form)
## cpu-total
### (cpu-total)
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
## create-window
### (create-window flags)
## def
### (def env var val [var val] ...)
## def?
### (def? var)
## defmacro
### (defmacro name vars body)
## defq
### (defq var val [var val] ...)
## div
### (div num num ...)
## each!
### (each! start end accum func seq)
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
## eval
### (eval form [env])
## fcos
### (fcos angle)
## fdiv
### (fdiv num num ...)
## ffi
### (ffi sym path flags)
## file-stream
### (file-stream path)
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
## inst-of
### (inst-of class obj)
## lambda
### (lambda ([param] ...) body)
## le
### (le num num ...)
## length
### (length seq)
## load
### (load path)
## lt
### (lt num num ...)
## macroexpand
### (macroexpand form)
## macroexpand-1
### (macroexpand-1 form)
## mail-mymail
### (mail-mymail)
## mail-send
### (mail-send obj mbox cpu)
## match?
### (match? list list)
## merge-sym
### (merge-sym list list)
## mod
### (mod num num ...)
## mul
### (mul num num ...)
## ne
### (ne num num ...)
## open-child
### (open-child path mode)
## open-farm
### (open-farm path num mode)
## open-pipe
### (open-pipe paths)
## open-remote
### (open-remote path cpu mode)
## pipe
### (pipe cmd)
## pipe-read
### (pipe-read pipe)
## pipe-write
### (pipe-write pipe str)
## pop
### (pop array)
## push
### (push array form ...)
## quasi-quote
### (quasi-quote arg)
## quote
### (quote arg)
## read
### (read stream last_char)
## read-char
### (read-char stream [width])
## read-line
### (read-line stream)
## repl
### (repl stream path)
## save
### (save str path)
## set
### (set env var val [var val] ...)
## setq
### (setq var val [var val] ...)
## slice
### (slice start end seq)
## some!
### (some! start end mode func seq)
## split
### (split str char)
## str
### (str arg)
## string-stream
### (string-stream str)
## sub
### (sub num num ...)
## sym
### (sym arg)
## task-mailbox
### (task-mailbox)
## throw
### (throw desc obj)
## time
### (time)
## while
### (while tst form ...)
## write
### (write stream str)
## write-char
### (write-char stream num [width])
## write-line
### (write-line stream str)
