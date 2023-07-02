# C Script expression compiler

The C script idea came along after I had started changing things over to use
the new Lisp engine to replace the NASM assembler. Partly to show off the power
that comes from having a Lisp as your macro assembler and partly to fulfill a
need to simplify prototyping code without having to worry about register
assignments and running out of registers etc.

You can freely mix and match lower level VP code and CScript expressions
provided you follow the rules, those rules will become clear as we go through
this document, but take a look at the `'canvas :fpoly` method to see what you
can do ! That's a mixture of CScript, VP register defs and `(method-input)`
calls !

As discussed in the `VP_ASSIGNMENT.md` document you call the CScript compiler
each time you use a string argument as an input or output to the `(assign)`
function.

The CScript compiler implementation files are `lib/asm/cscript.inc`,
`lib/asm/csopt.inc` and `lib/asm/scopes.inc`.

So what does the CScript compiler do ? It consists of 4 phases:

* Tokenization of the expression.

* Reverse polish conversion.

* Compilation to VP assembler instructions.

* Optimization of generated VP assembler.

I'm hoping this doc might demystify a few compiler things in general, but I'm
not a compiler expert. Just an amateur playing with fire...

It didn't occur to me before why it's called the `Shunting Yard Algorithm` but
now I see it ! If you had a train of carriages on a track and wanted to
rearrange them and you had a side track and a turntable, THIS is what you would
do !

Put the first class carriages before the others etc... and do it with
hierarchial rules... it's not far off the compositor visibility idea :) Maybe I
have an issue with 1D thinking.

## Tokenization

Tokenization is a fairly simple thing. The expression string is scanned
character by character and groups are created that represent particular types
of token. A list of those tokens and their types is returned as the tokenizer
output.

`(cscript-tokenize line)` is the function that does this work. The types of
output token are: `:symbol`, `:number`, `:string`, `:path`, `:label`, `:lrb`,
`:rrb`, `:lsb`, `:rsb` or `:operator`.

Let's see a few simple example expressions and what they get tokenized into.

```vdu
"1"
("1")
(:number)

"&id->str_data"
(":" "id" "->" "str_data")
(:operator :symbol :operator :symbol)

"this->canvas_cx"
("this" "->" "canvas_cx")
(:symbol :operator :symbol)

"vdu->vdu_char_height * 9 >> 4"
("vdu" "->" "vdu_char_height" "*" "9" ">>" "4")
(:symbol :operator :symbol :operator :number :operator :number)
```

## Reverse polish

I don't find this stage easy to describe ! But the general idea is that you are
rearranging your tokens and operations on those tokens into an easily digested,
left to right, sequence. Maybe you can see it as flattening an expression tree
into a list etc. It's also the stage where the precedence of various operators
are taken into account in how you flatten the tree.

The algorithm used here is the classic [`Shunting Yard
Algorithm`](https://brilliant.org/wiki/shunting-yard-algorithm/).

`(cscript-reverse-polish tokens)` is the function used to do the conversion.

Again let's see a few simple example expressions and what the reverse polish
conversion does to them.

```vdu
"char /= +char_semi"
(("char" "/=" "+char_semi")
(:symbol :operator :symbol))
...
(("char" "+char_semi" "/=")
(:symbol :symbol :operator))

"char > +char_space || char = -1"
(("char" ">" "+char_space" "||" "char" "=" "_" "1")
(:symbol :operator :symbol :operator :symbol :operator :operator :number))
...
(("char" "+char_space" ">" "char" "1" "_" "=" "||")
(:symbol :symbol :operator :symbol :number :operator :operator :operator))

"(table + len + +ptr_size) & - +ptr_size"
(("(" "table" "+" "len" "+" "+ptr_size" ")" "&" "_" "+ptr_size")
(:lrb :symbol :operator :symbol :operator :symbol :rrb :operator :operator :symbol))
...
(("table" "len" "+" "+ptr_size" "+" "+ptr_size" "_" "&")
(:symbol :symbol :operator :symbol :operator :symbol :operator :operator))
```

Notice how on that last example the entires for the `:lrb` and `:rrb` tokens
have mysteriously vanished... ;) That's because they don't mean anything other
than a change in order of the reverse polish output !

## Compilation

After you have converted your expression into reverse polish form the
compilation is as simple as:

```vdu
(defun cscript-compile (rpn_output)
	(each! 0 -1 (lambda (token type)
		(case type
			(:operator (compile-operator (sym token)))
			(:number (compile-const (str-as-num token)))
			(:symbol (compile-ref (sym token)))
			(:path (compile-bind (sym token)))
			(:label (compile-label (sym token)))
			(:string (compile-string token)))) rpn_output))
```

OK, that's a bit flippant. What the reverse polish stage did was to arrange
things so that we can scan the output and emit instructions without a care. It
also boiled away the ordering operators like `:lrb` and `:lsb` ! All of those
operators are just changing the order of what we do at the VP output stage.

Your only left with these operations, in your defined order. Languages may vary
in the order rules, but they ALL do this !

`:operator`, `:number`, `:symbol`, `:path`, `:label`, `:string`

Well, having said that, I'm not so sure about Lisp... or Forth. Any language
that lets you change the parser rules or compilation rules with a user function
or macro is in a world of it's own.

What holds it together if you can do that ? Discipline, and maybe learned
responses... Pavlov's dog etc. But be aware that you need to rise above that.

I have yet to get to the interesting bits though... better keep typing ;)

### Compilation stacks

In order to compile the reverse polish output from the CScript expression stage
we use a stack of symbols and their associated type. By type I mean whether the
register is holding a `ptr` or an `int` etc.

Items are added to this stack on operations such as `(compile-const)` and
`(compile-ref)` and are removed or transformed when operator actions are
compiled.

```vdu
(defq +vreg ''(_v0 _v1 _v2 _v3 _v4 _v5 _v6 _v7 _v8 _v9 _v10 _v11 _v12 _v13 _v14)
	*vregt* (list))
```

Functions and macros are provided to push, pop and extract items from the
stack.

```vdu
(defmacro set-type (_)
	(list 'elem-set -2 '*vregt* _))

(defmacro get-type ()
	'(elem-get -2 *vregt*))

(defmacro top-reg ()
	'(vreg-sym (dec (length *vregt*))))

(defmacro tmp-reg ()
	'(vreg-sym (length *vregt*)))

(defun push-reg (_)
	(vreg-sym (dec (length (push *vregt* _)))))

(defun pop-reg ()
	(list (vreg-sym (dec (length *vregt*))) (pop *vregt*)))
```

We also keep a list of the generated VP instructions, that we add to as we go,
and a macro to push items onto it for us.

```vdu
(defmacro add-inst (&rest b)
	`(push *inst* ~b))
```

### Code generation

Let's take a look at the `(compile-const)` function to see how we `stack` a
constant as we scan the reverse polish input. What we want to do is add a new
item onto the type stack, in this case its type is `:nil`. We also want to add
a new instruction to the output that will perform this operation.

```vdu
(defun compile-const (_)
	(add-inst (list 'vp-cpy-cr _ (push-reg :nil))))
```

We use `(push-reg) -> sym` to stack the type, `:nil`, and return the symbol
representing the register that will hold it, and output an instruction that
will look something like:

```vdu
(vp-cpy-cr 592 _v3)
```

Let's see what simple operators like addition and subtraction do.

```vdu
(defun compile-plus (_)
	(add-inst (list 'vp-add-rr (pop-value) (top-value))))

(defun compile-minus (_)
	(add-inst (list 'vp-sub-rr (pop-value) (top-value))))
```

These are simple operators that take a value from the stack and transform the
resulting top value. If you hit both of these one after the other you may see
something like the following instructions output:

```vdu
(vp-add-rr _v4 _v3)
(vp-sub-rr _v3 _v2)
```

Note the use of `(pop-value)` and `(top-value)` functions to refer to the
popped and top items on the stack ! This is where some of the magic starts to
happen. The type of the stack items are tested and extra instructions may be
generated to take that type into account. We may need to load a value from
memory into a register, that the addition and subtraction operators will act
on.

### Dereferencing

Look closer at the `(pop-value)` and `(top-value)` functions.

```vdu
(defun compile-deref ()
	(if (defq x (top-reg) w (get-type))
		(defq z (slice 1 -1 w) z (if (eql z "") :nil z) w (elem-get 0 w))
		(throw "No type info !" x))
	(set-type z)
	(setq w (elem-get (find-rev w "bBsSiIlLp")
		'(vp-cpy-ir-b vp-cpy-ir-ub vp-cpy-ir-s vp-cpy-ir-us
		vp-cpy-ir-i vp-cpy-ir-ui vp-cpy-ir vp-cpy-ir vp-cpy-ir)))
	(add-inst (list w x 0 x)))

(defun compile-deref? ()
	(if (get-type)
		(compile-deref)))

(defun pop-value ()
	(compile-deref?)
	(pop *vregt*)
	(vreg-sym (length *vregt*)))

(defun top-value ()
	(when (get-type)
		(compile-deref)
		(set-type :nil))
	(top-reg))
```

The stack items are tested to see if they are boring types like `:nil`, and if
not then the type of the item is used to output the correct VP memory read
operation. The type code for the item is trimmed by one element. These type
codes are simply a string, such as `i` for a signed integer `ppI` for pointer
to pointer to unsigned integer etc. If the type string becomes empty it is set
to `:nil` to represent the end of the line.

### Referencing

We push typed items onto the stack with the opposite function to
`(compile-deref)`, `(compile-ref)` !

Let's look at what compiling a `:symbol` does.

```vdu
(defun compile-ref (_)
	(cond
		((not (defq s (scope-get-sym _)))
			;not in symbol table so figure out what it is
			(cond
				((get (sym (str _ "_t")))
					;field/member
					(add-inst (list 'vp-cpy-cr
						_ (push-reg (eval (sym (str _ "_t")))))))
				((get _)
					;equate
					(compile-const _))
				(:t (throw "Symbol not defined !" _))))
		((eql 'var (elem-get 1 s))
			;variable
			(add-inst (list 'vp-lea-i :rsp
				(+ (scope-get (elem-get 0 s)) (elem-get 2 s))
				(push-reg (elem-get 3 s)))))
		(:t (throw "Symbol not a variable !" _))))
```

We examine what the symbol is, by first looking it up in the `(def-vars)`
scope. Fallback to some simple VP level types we want to cope with, and throw
an error if not found.

But let's say we do find it, then we output a VP instruction that creates a
reference to the stack location, and push the variables type string onto the
compilation stack.

Define a variable and reference it:

```vdu
(def-vars (int a b c))
(push-scope)
(assign {a + b} {c})
(pop-scope)
(return)
```

The compilation of `{a}` will stack a type of "i" and output a VP instruction
of:

```vdu
(vp-lea-i :rsp 0 _v0)
```

Later on when we need to perform the "+" operator we see the dereference. The
full example showing the output.

```vdu
(let ((*debug_inst* :t))
	(def-vars (int a b c))
	(push-scope)
	(assign {a + b} {c})
	(pop-scope)
	(return))

(vp-alloc 16)
pre opt:
	(vp-lea-i :rsp 0 _v0)
	(vp-lea-i :rsp 4 _v1)
	(vp-cpy-ir-i _v1 0 _v1)
	(vp-cpy-ir-i _v0 0 _v0)
	(vp-add-rr _v1 _v0)

	(vp-lea-i :rsp 8 _v1)
	(vp-cpy-ri-i _v0 _v1 0)
post opt:
	(vp-cpy-ir-i :rsp (+ 4 0) _v1)
	(vp-cpy-ir-i :rsp (+ 0 0) _v0)
	(vp-add-rr _v1 _v0)

	(vp-cpy-ri-i _v0 :rsp (+ 8 0))
(vp-free 16)
(vp-ret)
```

### Multiple statements

You may have multiple statements compiled in the same expression, using the ","
separator. What happens in that case is that each statement is compiled one
after the other and the results build up on the compilation stack.

This is extremely useful when we come to the way that the `(assign)` function
uses this to exchange between CScript and VP level source !

Take another example:

```vdu
(let ((*debug_inst* :t))
	(def-vars (int a b c d) (ushort x y))
	(push-scope)
	(assign {a + b, x - y} {c, d})
	(pop-scope)
	(return))

(vp-alloc 24)
pre opt:
	(vp-lea-i :rsp 0 _v0)
	(vp-lea-i :rsp 4 _v1)
	(vp-cpy-ir-i _v1 0 _v1)
	(vp-cpy-ir-i _v0 0 _v0)
	(vp-add-rr _v1 _v0)

	(vp-lea-i :rsp 16 _v1)
	(vp-lea-i :rsp 18 _v2)
	(vp-cpy-ir-us _v2 0 _v2)
	(vp-cpy-ir-us _v1 0 _v1)
	(vp-sub-rr _v2 _v1)

	(vp-lea-i :rsp 12 _v2)
	(vp-cpy-ri-i _v1 _v2 0)
	(vp-lea-i :rsp 8 _v1)
	(vp-cpy-ri-i _v0 _v1 0)
post opt:
	(vp-cpy-ir-i :rsp (+ 4 0) _v1)
	(vp-cpy-ir-i :rsp (+ 0 0) _v0)
	(vp-add-rr _v1 _v0)

	(vp-cpy-ir-us :rsp (+ 18 0) _v2)
	(vp-cpy-ir-us :rsp (+ 16 0) _v1)
	(vp-sub-rr _v2 _v1)

	(vp-cpy-ri-i _v1 :rsp (+ 12 0))
	(vp-cpy-ri-i _v0 :rsp (+ 8 0))
(vp-free 24)
(vp-ret)
```

Notice what has happened here. The results of the addition and subtraction sub
expressions have been stacked up into the results `_v0` and `_v1` ! Note that
the work carried out to do the subtraction sub expression does not use `_v0`,
it is not trashed ! Also note that we are not actually using 'real' VP
registers yet ! No mention of `:r0` or `:r1` etc. ;)

### Assign inputs and outputs

What we have built up above, on the input side of the `(assign)` is a
representation of the loads, stores and computation into a stack of virtual
virtual registers. What we then do on the output side of the `(assign)` is to
drain that stack of virtual virtual registers.

BUT, we could populate those stacks without using the CScript compiler ! The
above so far has always been a Script to Script assignment. Both the source and
drain code created by the compilation functions.

We could, and do, use just the input part or the output part of this process to
interface with VP register lists in order to call VP methods, or use output
from VP methods. We have the ability to define symbol bindings for those `_vXX`
symbols before we let the assembler see the list of VP instructions we have
built up !

A call to a VP method from CScript can gather the VP register inputs from the
`(method-input)` function and create a register map in order to create code
that will use exactly the correct registers for that call, likewise you use
`(method-output)` function to map output registers to a pre-loaded compiler
stack that a CScript output can consume.

This is what these register map functions provide.

```vdu
(defmacro reset-reg-stack (_)
	`(defq *inst* (push (clear '()) progn)
		*vregt* (slice 0 ,_ '(:nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil :nil))))

(defun def-reg-map (pre spill)
	(setd spill '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14))
	(each (# (def *func_env* %0 %1)) +vreg (if pre (merge-obj pre spill) spill)))
```

By default, if you don't provide a list of VP registers to `(def-reg-map)` it
will use the full list of VP registers. If you do provide a list it will merge
the full set with your provided list ! Your provided set will be at the front
and take priority. This is also why use of the CScript compiler in general
means you must accept that all VP registers may be trashed.

Take a look at `(assign-script-to-asm)` now with fresh eyes and these ideas in
mind.

```vdu
(defun assign-script-to-asm (src dst _)
	(unless (= (length (setq src (split src ","))) (length dst))
		(throw "Mismatching number of src/dst parameters !" (list src dst)))
	(when (/= 0 (length dst))
		(reset-reg-stack 0)
		(each (# (cscript %0) (compile-deref?)) src)
		(when *debug_inst*
			(print "pre opt:")
			(each (const print-inst) *inst*))
		(opt-inst-list *inst*)
		(when *debug_inst*
			(print "post opt:")
			(each (const print-inst) *inst*))
		(def-reg-map (cat dst) _)
		(eval *inst* *func_env*)))
```

* Split up the CScript expression by ",".

* Test it matches the VP destination register list.

* Reset the compiler register stack.

* Compile each of the CScript expressions, forcing a `(compile-deref?)` on any
hanging memory references.

* Call the CScript optimizer to crunch down what we can.

* Set the `_vXX` to `:rXX` bindings.

* Evaluate the compiler output instruction list.

The other types of `(assign)` are variations on this theme. In the
`VP_ASSIGNMENT.md` doc we talked about the 4 principle possibilities but there
are other things that could be done.

There are further extensions to the operators that could be added, user custom
operators are possible but have not yet been used.

Support was added to the compiler to directly work on ChrysaLisp fixed point
values using the "*>" and "</" operators.
