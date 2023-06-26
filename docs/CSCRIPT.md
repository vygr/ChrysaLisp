# C script expression compiler

The C script idea came along after I had started changing this over to use the
new Lisp engine to replace the NASM assembler. Partly to show off the power
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
`lib/asm/csopy.inc` and to some extent `lib/asm/code.inc`.

So what does the CScript compiler do ? It consists of 4 phases:

* Tokenization of the given string expression.

* Conversion to reverse polish form.

* Compilation to VP assembler instructions.

* Optimization of generated VP assembler.

## Tokenization

Tokenization is a fairly simple thing. The expression string is scanned
character by character and groups are created that represent particular types
of token. A list of those tokens and their types is returned as the tokenizer
output.

`(cscript-tokenize line)` is the function that does this work. The types of
output token are: `:symbol`, `:number`, `:string`, `:path`, `:label` or
`:operator`.

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
