# lisp

## obj

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
:r4 = minimum number of args (int)
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
:r3 = signiture pointer (pushort)
:r4 = number of args (int)
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
:r4 = minimum number of args (int)
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
;lisp binding
;(bind (param ...) seq)
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
:r2 = stdin stream object (ptr)
:r3 = stdout stream object (ptr)
:r4 = stderr stream object (ptr)
outputs
:r0 = lisp object object (ptr)
:r1 = 0 if error, else ok
trashes
:r1-:r14
```

### :lisp_apply -> class/lisp/lisp_apply

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(apply lambda list)
```

### :lisp_bind -> class/lisp/lisp_bind

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_catch -> class/lisp/lisp_catch

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(catch form eform)
```

### :lisp_cond -> class/lisp/lisp_cond

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(cond [(tst [body])] ...)
```

### :lisp_copy -> class/lisp/lisp_copy

### :lisp_env_pop -> class/lisp/lisp_env_pop

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(env-pop [env])
```

### :lisp_env_push -> class/lisp/lisp_env_push

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(env-push [env])
```

### :lisp_eql -> class/lisp/lisp_eql

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(eql form form)
```

### :lisp_eval -> class/lisp/lisp_eval

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(eval form [env])
```

### :lisp_ffi -> class/lisp/lisp_ffi

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(ffi sym path flags)
```

### :lisp_if -> class/lisp/lisp_if

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(if tst form [else_form])
```

### :lisp_lambda -> class/lisp/lisp_lambda

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_macro -> class/lisp/lisp_macro

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_macroexpand -> class/lisp/lisp_macroexpand

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(macroexpand form)
```

### :lisp_mcall -> class/lisp/lisp_mcall

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(. env sym [...])
```

### :lisp_prebind -> class/lisp/lisp_bindfun

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(prebind form)
```

### :lisp_prin -> class/lisp/lisp_prin

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_print -> class/lisp/lisp_print

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_progn -> class/lisp/lisp_progn

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
```

### :lisp_qquote -> class/lisp/lisp_qquote

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(quasi-quote form)
```

### :lisp_quote -> class/lisp/lisp_quote

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(quote form)
```

### :lisp_read -> class/lisp/lisp_read

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(read stream last_char)
```

### :lisp_repl -> class/lisp/lisp_repl

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(repl stream path)
```

### :lisp_throw -> class/lisp/lisp_throw

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(throw str form)
```

### :lisp_while -> class/lisp/lisp_while

```code
inputs
:r0 = lisp object (ptr)
:r1 = args list object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = return value object (ptr)
trashes
:r1-:r14
;lisp binding
;(while tst body)
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
;lisp binding
;unexpected )
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
;lisp binding
;(lambda ([arg ...]) body)
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

### :run -> class/lisp/run

```code
lisp run loop task
inputs
msg of lisp filename
```

### :vtable -> class/lisp/vtable

