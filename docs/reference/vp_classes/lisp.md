# :lisp

```image
docs/reference/vp_classes/lisp.cwb
```

## :obj

## Lisp Bindings

### (apply lambda seq) -> form

### (bind (sym ...) seq) -> val

### (catch form eform) -> 'form

### (cond [(tst body)] ...) -> 'form

### (condn [(tst body)] ...) -> 'form

### (env-pop) -> 'env

### (env-push [env]) -> 'env

### (eval form [env]) -> 'form

### (eval-list list [env]) -> list

### (ffi path [sym flags])

### (identity [form]) -> :nil | form

### (if tst form [else_form ...]) -> 'form

### (ifn tst form [else_form ...]) -> 'form

### (macroexpand form) -> 'form

### (. env sym [...]) -> form

### (prebind form) -> form

### (prin [form] ...) -> :nil

### (print [form] ...) -> :nil

### (progn [body]) -> 'form

### (quasi-quote form)

### (quote form)

### (read stream [last_char]) -> :nil | (form next_char)

### (repl stream name) -> form

### (repl-info) -> (name line)

### (throw str form)

### (until tst [body]) -> tst

### (while tst [body]) -> :nil

## VP methods

### :create -> class/lisp/create

```code
inputs
:r0 = script string object (ptr)
:r1 = stdin stream object (ptr)
:r2 = stdout stream object (ptr)
:r3 = stderr stream object (ptr)
outputs
:r0 = 0 if error, else lisp object (ptr)
trashes
:r0-:r14, :f0-:f15
```

### :deinit -> class/lisp/deinit

```code
inputs
:r0 = lisp object (ptr)
outputs
:r0 = lisp object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :env_args_match -> class/lisp/env_args_match

```code
inputs
:r1 = args list object (ptr)
:r2 = vtable pointer (ptr)
:r3 = min number of args (int)
outputs
:r1 = args list object (ptr)
:r2 = 0 if error, else ok
trashes
:r2-:r5
```

### :env_args_sig -> class/lisp/env_args_sig

```code
inputs
:r1 = args list object (ptr)
:r2 = signature pointer (pshort)
:r3 = min number of args (int)
:r4 = max number of args (int)
outputs
:r1 = args list object (ptr)
:r2 = 0 if error, else ok
trashes
:r2-:r6
```

### :env_args_type -> class/lisp/env_args_type

```code
inputs
:r1 = args list object (ptr)
:r2 = vtable pointer (ptr)
:r3 = min number of args (int)
outputs
:r1 = args list object (ptr)
:r2 = 0 if error, else ok
trashes
:r2-:r5
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
:r1-:r14, :f0-:f15
```

### :env_pop -> class/lisp/env_pop

```code
inputs
:r0 = lisp object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = hmap object (ptr)
trashes
:r1-:r14, :f0-:f15
info
if no other has a ref to the environment, and it is as it was made,
a single bucket that has not grown, its keys and values are dropped,
in line where they live on, and it goes on the chain of empty ones,
the link held as its parent. If not it is given a deref, as before.
```

### :env_push -> class/lisp/env_push

```code
inputs
:r0 = lisp object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = hmap object (ptr)
trashes
:r1-:r5, :r14, :f0-:f15
info
the new environment is taken from the chain of empty ones the lisp
object keeps, :env_pop puts them there, so most calls of a lambda
need not create one.
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
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
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
:r1-:r8, :r14, :f0-:f15
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
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
```

### :repl_bind -> class/lisp/repl_bind

```code
inputs
:r0 = lisp object (ptr)
:r1 = form object iter (pptr)
outputs
:r0 = lisp object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :repl_error -> class/lisp/repl_error

```code
inputs
:r1 = error payload object (ptr)
:r2 = description c string (pubyte)
:r3 = 0, else error msg number (uint)
outputs
:r0 = lisp object (ptr)
:r1 = error object (ptr)
trashes
:r1-:r8, :r14, :f0-:f15
info
the lisp object is the one the task holds, in its tcb. This is cold
code, and so a caller need not keep its lisp object to hand, on the
stack say, just in case it has an error.
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
:r1-:r14, :f0-:f15
info
this is the heart of the recursion, so what it holds on the stack,
over each call it makes, is kept to the least. A symbol, or a form
that evals to itself, uses none. A special form is a jump, so none. A
built in function holds the form and the args while the args eval,
and only the args while it runs. The list for the args is taken from,
and given back to, a chain of empty ones. A lambda holds the form and the args
while the args eval, and nothing while its body runs.

The function of a form is nearly always prebound, a func object or a
lambda list, held by the form. So it is not given a ref, and is found
from the form when wanted, with no slot of its own. Only a function
that has to be evaluated, the general case, has a slot, and a ref.
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
:r1-:r14, :f0-:f15
```

### :repl_expand -> class/lisp/repl_expand

```code
inputs
:r0 = lisp object (ptr)
:r1 = form object iter (pptr)
outputs
:r0 = lisp object (ptr)
trashes
:r1-:r14, :f0-:f15
```

### :repl_print -> class/lisp/repl_print

```code
inputs
:r0 = lisp object (ptr)
:r1 = stream object (ptr)
:r2 = value (ptr)
:r3 = trunc flag (uint)
outputs
:r0 = lisp object (ptr)
trashes
:r1-:r14, :f0-:f15
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
:r1-:r14, :f0-:f15
info
the last form is a jump to its eval, with nothing left on the stack.
The frame is only for the forms before it, and has no slot for the
lisp object, it stays in :r0, each eval gives it back.
```

### :run -> class/lisp/run

```code
lisp run loop task
inputs
msg of lisp filename
trashes
:r0-:r14, :f0-:f15
```

### :type -> class/lisp/type

```code
inputs
:r0 = lisp object (ptr)
outputs
:r0 = lisp object (ptr)
:r1 = type list object (ptr)
trashes
:r1-:r5, :f0-:f15
```

### :vtable -> class/lisp/vtable

