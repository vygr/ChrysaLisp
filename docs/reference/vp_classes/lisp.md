# lisp

## obj

## Lisp Bindings

### (apply lambda seq)

### (catch form eform)

### (cond [(tst [body])] ...)

### (condn [(tst [body])] ...)

### (env-pop [env])

### (env-push [env])

### (eql form form)

### (eval form [env])

### (ffi path [sym flags])

### (if tst form [else_form])

### (ifn tst form [else_form])

### (macroexpand form)

### (. env sym [...])

### (prebind form)

### (quasi-quote form)

### (quote form)

### (read stream [last_char])

### (repl stream path)

### (throw str form)

### (until tst [body])

### (while tst [body])

## VP methods

### :create -> class/lisp/create

### :deinit -> class/lisp/deinit

```code
inputs
:r0 = lisp object (ptr)
outputs
:r0 = lisp object (ptr)
trashes
:r1-:r14
```

### :env_args_match -> class/lisp/env_args_match

```code
inputs
:r1 = args list object (ptr)
:r3 = vtable pointer (ptr)
:r4 = min number of args (int)
outputs
:r2 = 0 if error, else ok
trashes
:r2-:r7
```

### :env_args_set -> class/lisp/env_args_set

```code
inputs
:r0 = args list object (ptr)
:r3 = args offset (uint)
:r5 = args dest (ptr)
trashes
:r0-:r5
```

### :env_args_sig -> class/lisp/env_args_sig

```code
inputs
:r1 = args list object (ptr)
:r3 = signature pointer (pushort)
:r4 = min number of args (int)
:r5 = max number of args (int)
outputs
:r2 = 0 if error, else ok
trashes
:r2-:r7
```

### :env_args_type -> class/lisp/env_args_type

```code
inputs
:r1 = args list object (ptr)
:r3 = vtable pointer (ptr)
:r4 = min number of args (int)
outputs
:r2 = 0 if error, else ok
trashes
:r2-:r7
```

### :env_bind -> class/lisp/env_bind

```code
inputs
:r0 = lisp object (ptr)
:r1 = vars list object (ptr)
:r2 = vals seq object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
lisp binding
(bind (param ...) seq)
```

### :env_pop -> class/lisp/env_pop

```code
inputs
:r0 = lisp object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = hmap object (ptr)
trashes
:r1-:r14
```

### :env_push -> class/lisp/env_push

```code
inputs
:r0 = lisp object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = hmap object (ptr)
trashes
:r1-:r14
```

### :init -> class/lisp/init

```code
inputs
:r0 = lisp object object (ptr)
:r1 = vtable (pptr)
:r2 = script string object (ptr)
:r3 = stdin stream object (ptr)
:r4 = stdout stream object (ptr)
:r5 = stderr stream object (ptr)
outputs
:r0 = lisp object object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :read -> class/lisp/read

```code
inputs
:r0 = lisp object (ptr)
:r1 = stream object (ptr)
:r2 = next char (uint)
outputs
:r0 = lisp object (ptr)
:r1 = form object (ptr)
:r2 = next char (uint)
trashes
:r1-:r14
lisp binding
unexpected )
```

### :read_char -> class/lisp/read_char

```code
inputs
:r0 = lisp object (ptr)
:r1 = stream object (ptr)
:r2 = last char (uint)
outputs
:r0 = lisp object (ptr)
:r1 = next char (uint)
trashes
:r1-:r14
```

### :read_num -> class/lisp/read_num

```code
inputs
:r0 = lisp object (ptr)
:r1 = stream object (ptr)
:r2 = next char (uint)
outputs
:r0 = lisp object (ptr)
:r1 = num object (ptr)
:r2 = next char (uint)
trashes
:r1-:r14
```

### :read_quasi -> class/lisp/read_quasi

```code
inputs
:r0 = lisp object (ptr)
:r1 = stream object (ptr)
:r2 = next char (uint)
:r3 = sym object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = list object (ptr)
:r2 = next char (uint)
trashes
:r1-:r14
```

### :read_str -> class/lisp/read_str

```code
inputs
:r0 = lisp object (ptr)
:r1 = stream object (ptr)
:r2 = close char (uint)
outputs
:r0 = lisp object (ptr)
:r1 = str object (ptr)
:r2 = next char (uint)
trashes
:r1-:r14
```

### :read_sym -> class/lisp/read_sym

```code
inputs
:r0 = lisp object (ptr)
:r1 = stream object (ptr)
:r2 = next char (uint)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
:r2 = next char (uint)
trashes
:r1-:r14
```

### :repl_apply -> class/lisp/repl_apply

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
:r2 = func object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
lisp binding
(lambda ([arg ...]) body)
```

### :repl_bind -> class/lisp/repl_bind

```code
inputs
:r0 = lisp object (ptr)
:r1 = form object iter (pptr)
outputs
:r0 = lisp object (ptr)
trashes
:r1-:r14
```

### :repl_error -> class/lisp/repl_error

```code
inputs
:r0 = lisp object (ptr)
:r1 = description c string (pubyte)
:r2 = 0, else error msg number (uint)
:r3 = error payload object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = error object (ptr)
trashes
:r1-:r14
```

### :repl_eval -> class/lisp/repl_eval

```code
inputs
:r0 = lisp object (ptr)
:r1 = form object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :repl_eval_list -> class/lisp/repl_eval_list

```code
inputs
:r0 = lisp object (ptr)
:r1 = list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :repl_expand -> class/lisp/repl_expand

```code
inputs
:r0 = lisp object (ptr)
:r1 = form object iter (pptr)
outputs
:r0 = lisp object (ptr)
trashes
:r1-:r14
```

### :repl_print -> class/lisp/repl_print

```code
inputs
:r0 = lisp object (ptr)
:r1 = stream object (ptr)
:r2 = value
outputs
:r0 = lisp object (ptr)
trashes
:r1-:r14
```

### :repl_progn -> class/lisp/repl_progn

```code
inputs
:r0 = lisp object (ptr)
:r1 = initial value object (ptr)
:r2 = list iter_begin (pptr)
:r3 = list iter_end (pptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :run -> class/lisp/run

```code
lisp run loop task
inputs
msg of lisp filename
```

### :vtable -> class/lisp/vtable

