# C script expression compiler

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
`lib/asm/csopt.inc` and to some extent `lib/asm/code.inc`.

So what does the CScript compiler do ? It consists of 4 phases:

* Tokenization of the expression.

* Reverse polish conversion.

* Compilation to VP assembler instructions.

* Optimization of generated VP assembler.

I'm hoping this doc might demystify a few compiler things in general, but I'm
not a compiler expert. Just a amateur playing with fire...

It didn't occur to me before why it's called the `Shunting Yard algorithm`...
but now I see it ! If you had a train of carriages on a track and wanted to
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

The algorithm used here is the classic `Shunting Yard Algorithm` !
`https://brilliant.org/wiki/shunting-yard-algorithm/`

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
things so that we can scan the output and emit instructions without a care. And
it also boiled away the ordering operators like `:lrb` and `:lsb` ! All of
those operators are just changing the order of what we do at the VP level.

Your left only with these operations, in the your defined order. Languages may
vary in the order rules, but they ALL do this !

`:operator`, `:number`, `:symbol`, `:path`, `:label`, `:string`

Well, having said that, I'm not so sure about Lisp... or Forth...
