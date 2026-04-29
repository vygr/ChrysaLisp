# CScript Compiler and the `assign` Statement

The CScript compiler is a lightweight expression compiler embedded in the
ChrysaLisp macro assembler. It allows C-style arithmetic and memory-access
expressions to be written as strings inside VP assembler source and compiled
directly to VP instructions. Its primary integration point is the `(assign)`
function, which dispatches between four code-generation modes depending on
whether its source and destination arguments are strings (CScript expressions)
or Lisp lists (raw VP register/memory descriptors).

Implementation files:

- `lib/asm/cscript.inc` - tokenizer, reverse-polish converter, code generator,
  operator table.

- `lib/asm/csopt.inc` - peephole optimizer that post-processes the generated VP
  instruction list.

- `lib/asm/assign.inc` - the `(assign)` dispatcher and all four assign variants.

- `lib/asm/scopes.inc` - operator scope/precedence registry used by the
  compiler.

## Compilation Pipeline

The compiler transforms an expression string through four sequential phases.

### Phase 1 - Tokenization (`cscript-tokenize`)

`(cscript-tokenize line)` scans the expression character by character,
classifying each run of characters into one of the following token types:

- `:symbol` - identifier, constant name, or type-cast keyword (`int`, `ptr`,
  `real`, `fixed`, `byte`, `ubyte`, `short`, `ushort`, `uint`, `long`,
  `pptr`).

- `:number` - numeric literal (decimal or other bases accepted by
  `+char_class_digit_base`).

- `:string` - a `"..."` string literal (introduced by `\q`).

- `:path` - a `$...` path literal (introduced by `$`).

- `:label` - a `@...` label reference (introduced by `@`).

- `:lrb` / `:rrb` - left/right round bracket `( )`.

- `:lsb` / `:rsb` - left/right square bracket `[ ]`.

- `:operator` - any operator character sequence, including multi-character
  operators such as `->`, `>>`, `||`, etc.

Unary context is tracked with the `unary` flag. When the tokenizer is in a
unary position, the leading characters `-`, `*`, `&`, `~`, `!` are remapped to
the internal unary tokens `_`, `#`, `:`, `~`, `!` respectively so the
reverse-polish stage can distinguish them from their binary counterparts.

Type-cast keywords (`int`, `ptr`, `real`, `fixed`, etc.) are recognised inside
the symbol scanner and returned with type `:operator` rather than `:symbol`.

The output of this phase is a pair of parallel lists: `out_tokens` and
`out_types`.

---

### Phase 2 - Reverse Polish Conversion (`cscript-reverse-polish`)

`(cscript-reverse-polish tokenize_output)` implements the classic Shunting
Yard Algorithm to reorder the flat token stream into Reverse Polish Notation
(RPN), respecting operator precedence and associativity.

Key behaviours:

- `:lrb` / `:lsb` tokens are pushed onto the operator stack and act as
  precedence barriers.

- `:rrb` drains the operator stack back to the matching `(`, which is then
  discarded.

- `:rsb` drains back to the matching `[`, discards it, then emits the
  synthetic `[]` subscript operator.

- `:operator` tokens consult the scope registry (via `scope-get`) for their
  precedence value. Higher-precedence pending operators are flushed to the
  output before the new operator is pushed.

- `:number`, `:symbol`, `:string`, `:path`, `:label` tokens pass straight
  through to the output lists unchanged.

After all tokens are processed the remaining operator stack is flushed. The
bracket tokens vanish entirely from the output; they exist only to control
ordering. The result is again a pair of parallel lists (`out_tokens`,
`out_types`) in RPN order, ready for linear left-to-right code generation.

---

### Phase 3 - Code Generation (`cscript-compile`)

`(cscript-compile rpn_output)` iterates the RPN lists and dispatches each
token to a compile function:

- `:number` calls `(compile-const)`, emits `vp-cpy-cr` and pushes a `:nil`
  type entry.

- `:symbol` calls `(compile-ref)`, looks the symbol up in the `(def-vars)`
  scope or the global environment; emits `vp-lea-i` for stack variables, or
  `vp-cpy-cr` for equates/constants; pushes the variable's type string.

- `:path` calls `(compile-bind)`, emits `fn-bind`.

- `:label` calls `(compile-label)`, emits `vp-lea-p`.

- `:string` calls `(compile-string)`, emits `fn-string`.

- `:operator` calls `(compile-operator)`, dispatches through the scope
  registry to the appropriate compile function (see Operator Table below).

#### The Virtual Register Stack

The compiler maintains two parallel mutable variables during code generation:

- `*inst*` - the growing list of VP instructions being emitted.

- `*vregt*` - a type stack whose length encodes how many virtual registers are
  in use, and whose elements record the type string of each.

Virtual registers are named `_v0` through `_v14`. They are not real VP
registers at this stage; they are symbols that are bound to actual VP registers
(`:r0` through `:r14`) only after the optimizer runs, via `(def-reg-map)`.

Key stack operations:

- `(push-reg type)` - appends `type` to `*vregt*` and returns the `_vN`
  symbol for that slot.

- `(pop-reg)` - removes and returns the top `(_vN type)` pair.

- `(top-reg)` / `(get-type)` - inspect the top of the stack without removing.

- `(set-type t)` - update the type of the top slot in place.

- `(tmp-reg)` - returns the `_vN` symbol one above the current top, used as
  a scratch register without officially growing the stack.

- `(reset-reg-stack n)` - re-initialises `*inst*` and `*vregt*` with `n`
  pre-populated `:nil` slots (used when the input side is VP registers).

#### Dereferencing and the Type String

Every stack entry carries a type string that encodes the chain of indirections
still pending. The characters map to VP load instructions as follows:

- `b` - signed byte, uses `vp-cpy-ir-b`

- `B` - unsigned byte, uses `vp-cpy-ir-ub`

- `s` - signed short, uses `vp-cpy-ir-s`

- `S` - unsigned short, uses `vp-cpy-ir-us`

- `i` - signed int, uses `vp-cpy-ir-i`

- `I` - unsigned int, uses `vp-cpy-ir-ui`

- `l` - long, uses `vp-cpy-ir`

- `p` - ptr, uses `vp-cpy-ir`

- `f` - real (IEEE double), uses `vp-cpy-if`

- `x` - fixed point (48.16), uses `vp-cpy-ir`

`(compile-deref)` reads the first character of the type string, emits the
corresponding load, and trims the first character off. When the type string
becomes empty it is set to `:nil`, signalling a fully-dereferenced scalar.

`(compile-deref?)` is a no-op when the type is already `:nil`.

`(pop-value)` calls `(compile-deref?)` then pops the stack.

`(top-value)` calls `(compile-deref)` on a non-nil top and sets its type to
`:nil`, leaving the register on the stack.

---

### Phase 4 - Optimization (`opt-inst-list`)

After code generation `(opt-inst-list *inst*)` performs a forward linear scan
of the instruction list applying several peephole rewrites. NOPed instructions
are replaced with `(lambda)` (the `+opt_nop` sentinel) and skipped by the
evaluator.

#### 4a - Variable Load / Offset Folding

For any memory read instruction (`+opt_read_ops`: `vp-cpy-ir`, `vp-cpy-ir-b`,
`vp-cpy-ir-s`, `vp-cpy-ir-i`, etc.):

- Scan backwards for a `(vp-lea-i :rsp _ Rn)` or `(vp-add-cr _ Rn)` that
  defines the base register `Rn` without an intervening use.

- If the `vp-lea-i` form is found, rewrite the read to address `:rsp` directly
  with a composite offset `(+ lea_offset read_offset)` and NOP the `vp-lea-i`.

- If the `vp-add-cr` form is found, fold its constant into the read offset and
  NOP the add, then re-visit the current instruction.

The same pattern is applied symmetrically for memory write instructions
(`+opt_write_ops`: `vp-cpy-ri`, `vp-cpy-ri-b`, `vp-cpy-ri-s`, `vp-cpy-ri-i`),
folding the base register computation into the write's base/offset fields.

#### 4b - Strength Reduction

`(vp-mul-cr K Rn)` where `K` is an exact power of two is replaced by
`(vp-shl-cr log2(K) Rn)`.

#### 4c - Constant Propagation

For any register-register arithmetic instruction (`+opt_rr_ops`: `vp-add-rr`,
`vp-sub-rr`, `vp-mul-rr`, `vp-and-rr`, `vp-or-rr`, `vp-xor-rr`, `vp-shl-rr`,
`vp-shr-rr`, `vp-asr-rr`):

- Scan backwards for a `(vp-cpy-cr K Rsrc)` that last defined the source
  register without intervening clobber.

- Rewrite to the constant-register form (`+opt_cr_ops`: `vp-add-cr`, etc.) and
  NOP the constant load.

#### 4d - Arithmetic Reassignment and Constant Folding

For any constant-register instruction (`+opt_cr_ops`):

- Scan backwards for either a `(vp-cpy-cr K Rdst)` or the same `cr` opcode
  operating on `Rdst`.

- In the first case fold the two constants together with the arithmetic
  operator and collapse to a single `vp-cpy-cr`.

- In the second case chain the constants into the existing `cr` instruction
  and NOP the older one.

#### 4e - Constant Fusion

For `vp-add-cr` / `vp-sub-cr` applied to `Rdst`:

- Scan backwards for a prior `vp-add-cr` or `vp-sub-cr` on the same `Rdst`.

- Fuse the two constants (respecting sign flip when mixing add/sub) into a
  single instruction and NOP the older one.

#### 4f - Compare Constant Forwarding

For any register-register comparison (`+opt_sxx_rr_ops`: `vp-seq-rr`,
`vp-sne-rr`, `vp-sle-rr`, `vp-sge-rr`, `vp-sgt-rr`, `vp-slt-rr`):

- Scan backwards for a `(vp-cpy-cr K Rsrc)` defining the comparand.

- If `K` fits in a signed 32-bit immediate, rewrite to the constant form
  (`+opt_sxx_cr_ops`) and NOP the load.

## Operator Table

Operators are registered in the scope system via `(scope-operator name prec
assoc fn)`. Lower precedence numbers bind tighter. Associativity `1` means
right-associative, `0` means left-associative.

Precedence 0, right-associative - type cast operators:

- `ptr`, `byte`, `ubyte`, `short`, `ushort`, `int`, `uint`, `long`, `pptr`,
  `real`, `fixed` - all call `compile-cast`, which sets the top-of-stack type
  string to the corresponding single-character code.

Precedence 1, left-associative - member/index access:

- `.` calls `compile-field` - struct field offset add without dereferencing.

- `->` calls `compile-member` - pointer member access: deref then offset add.

- `[]` calls `compile-index` - array subscript: deref then offset add.

Precedence 2, right-associative - unary operators:

- `:` calls `compile-uaddrof` - address-of, clears the type to `:nil`.

- `_` calls `compile-uminus` - unary negate via `vp-mul-cr -1`.

- `#` calls `compile-uderef` - explicit extra dereference.

- `~` calls `compile-unot` - bitwise NOT via `vp-xor-cr -1`.

- `!` calls `compile-ulnot` - logical NOT via `vp-lnot-rr`.

Precedence 3, left-associative - multiplicative operators:

- `*>` calls `compile-fmul` - fixed-point multiply (multiply then `asr 16`).

- `</` calls `compile-fdiv` - fixed-point divide (`shl 16` dividend then
  divide).

- `*` calls `compile-mul` - integer multiply via `vp-mul-rr`.

- `/` calls `compile-divu` - unsigned divide via `vp-div-rrr-u`.

- `%` calls `compile-remu` - unsigned remainder.

- `//` calls `compile-div` - signed divide.

- `%%` calls `compile-rem` - signed remainder.

Precedence 4, left-associative - additive operators:

- `+` calls `compile-plus` via `vp-add-rr`.

- `-` calls `compile-minus` via `vp-sub-rr`.

Precedence 5, left-associative - shift operators:

- `<<` calls `compile-lshift` via `vp-shl-rr`.

- `>>` calls `compile-rshift` via `vp-shr-rr`.

- `>>>` calls `compile-arshift` via `vp-asr-rr`.

Precedence 6, left-associative - relational operators:

- `<` calls `compile-cmp` via `vp-slt-rr`.

- `>` calls `compile-cmp` via `vp-sgt-rr`.

- `<=` calls `compile-cmp` via `vp-sle-rr`.

- `>=` calls `compile-cmp` via `vp-sge-rr`.

Precedence 7, left-associative - equality operators:

- `=` calls `compile-cmp` via `vp-seq-rr`.

- `/=` calls `compile-cmp` via `vp-sne-rr`.

Precedence 8, left-associative:

- `&` calls `compile-and` via `vp-and-rr`.

Precedence 9, left-associative:

- `^` calls `compile-xor` via `vp-xor-rr`.

Precedence 10, left-associative:

- `|` calls `compile-or` via `vp-or-rr`.

Precedence 11, left-associative:

- `&&` calls `compile-land` via `vp-land-rr`.

Precedence 12, left-associative:

- `||` calls `compile-lor` via `vp-or-rr`.

Precedence 13, left-associative:

- `=>` calls `compile-arrow` - store operator: pops the destination address,
  pops the source value, selects the correct `vp-cpy-ri-*` variant from the
  destination type string, and emits the store.

Precedence 14 - grouping only, consumed entirely by the RPN stage:

- `(`, `)`, `[`, `]`

## The `assign` Statement

`(assign src dst)` is the central code-generation statement of the VP assembler.
Both `src` and `dst` can independently be either a string (CScript expression)
or a list (VP register/memory descriptors). The four resulting combinations each
call a different internal function.

The dispatch logic in `assign`:

- `src` is a string and `dst` is a string - calls `assign-script-to-script`.

- `src` is a string and `dst` is a list - calls `assign-script-to-asm`.

- `src` is a list and `dst` is a string - calls `assign-asm-to-script`.

- `src` is a list and `dst` is a list - calls `assign-asm-to-asm`.

---

### Mode 1 - ASM to ASM (`assign-asm-to-asm`)

Both `src` and `dst` are Lisp lists of VP register/memory descriptors. No
CScript compilation takes place. The function:

- Filters out ignored pairs where the destination equals the source, or
  destination is `_`.

- For lists longer than one element calls `(assign-topology-sort)` to sequence
  the copies so that a register used as a source is never overwritten before it
  has been read. If a circular dependency is detected a hard error is thrown.

- Iterates each `(src, dst)` pair, classifying each via `(assign-src-type)`
  and `(assign-dst-type)` and emitting the exact single VP instruction required
  (e.g. `vp-cpy-rr`, `vp-cpy-ir`, `vp-cpy-ri-b`, `vp-lea-i`, `fn-bind`,
  etc.).

Source type codes returned by `(assign-src-type)`:

- `:r` - GP register.

- `:f` - FP register.

- `:c` - constant or equate.

- `:s` - string literal.

- `:i` - indirect `(reg offset)`, long width.

- `:ii` - indirect signed int.

- `:iui` - indirect unsigned int.

- `:ius` - indirect unsigned short.

- `:iub` - indirect unsigned byte.

- `:is` - indirect signed short.

- `:ib` - indirect signed byte.

- `:d` - double-register indirect `(reg reg)`.

- `:&i` - address-of `(& reg offset)`.

- `:&d` - address-of double-register.

- `:$` - label or code pointer.

- `:@` - bound symbol.

---

### Mode 2 - Script to ASM (`assign-script-to-asm`)

`src` is a CScript string and `dst` is a list of VP destination registers.

- `src` is split on `,` to produce one sub-expression per destination.

- Lengths are checked to match `dst`.

- `(reset-reg-stack 0)` initialises a fresh empty virtual register stack and
  empty instruction list.

- Each sub-expression is compiled with `(cscript)` followed by
  `(compile-deref?)` to ensure any pending memory reference is resolved to a
  value register.

- `(opt-inst-list *inst*)` runs the peephole optimizer over the accumulated
  instructions.

- `(def-reg-map (cat dst) %2)` binds each `_vN` symbol to the corresponding
  real VP register from `dst`. The optional third argument provides a priority
  prefix list.

- `(eval *inst* *func_env*)` executes the instruction list, emitting the final
  VP assembly.

---

### Mode 3 - ASM to Script (`assign-asm-to-script`)

`src` is a list of VP source registers and `dst` is a CScript string
describing stores.

- `dst` is split on `,` and filtered for `"_"` ignores.

- `(reset-reg-stack (length src))` pre-fills the virtual register stack with
  one `:nil` slot per source register so the compiler sees pre-loaded values.

- Each destination sub-expression is compiled with `(cscript)` and then
  `(compile-arrow)` to emit the store.

- The optimizer runs, then `(def-reg-map (reverse src) %2)` binds the virtual
  registers to the source VP registers (reversed to match stack order).

- `(eval *inst* *func_env*)` emits the final VP assembly.

---

### Mode 4 - Script to Script (`assign-script-to-script`)

Both `src` and `dst` are CScript strings. This is the pure CScript path.

- `src` is split on `,` and `dst` is split on `,` and trimmed.

- Lengths are checked.

- `(reset-reg-stack 0)` starts a fresh stack.

- All source sub-expressions are compiled in order with `(cscript)`, building
  up the virtual register stack with all results.

- The destination sub-expressions are compiled in reverse order with `(reach)`,
  each followed by `(compile-arrow)`, draining the stack in the correct order.

- The optimizer runs, then `(def-reg-map :nil %2)` binds `_vN` to the standard
  default VP register sequence (`:r0` through `:r14`).

- `(eval *inst* *func_env*)` emits the final VP assembly.

## Multiple Expressions and the `,` Separator

A single CScript string may contain multiple independent sub-expressions
separated by `,`. Each sub-expression is compiled sequentially and its result
is pushed onto the virtual register stack independently. This allows:

```vdu
(assign {a + b, x - y} {c, d})
```

to compute both expressions before any stores occur, so the results are each
in their own `_vN` register. The store phase then drains them in reverse to
match the destination list, avoiding any register collision between the two
computed values.

## Virtual Register Lifetime and Real Register Binding

Throughout phases 1 through 4 the CScript compiler uses only the symbolic
names `_v0` through `_v14`. No real VP register names appear in the generated
instruction list. The binding step `(def-reg-map)` happens after optimization:

```vdu
(defun def-reg-map (pre spill)
	(setd spill '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7
		:r8 :r9 :r10 :r11 :r12 :r13 :r14))
	(each (# (deffvar %0 %1)) +vreg (if pre (merge pre spill) spill)))
```

- The default spill list is the full set of VP integer registers.

- If `pre` (a caller-supplied priority list, such as the actual destination
  registers from the `dst` list) is provided, it is merged to the front of the
  default list.

- Each `_vN` symbol is bound in the assembler environment to the Nth register
  from that merged list.

This is why any use of the CScript compiler implicitly allows all VP registers
to be clobbered. The compiler can allocate any register it needs. If the caller
must preserve specific registers they must be saved and restored explicitly in
surrounding VP code.

## Fixed-Point Support

Two operators directly support the ChrysaLisp `fixed` type (48.16 fixed-point,
stored as a 64-bit integer with 16 fractional bits):

- `*>` (`compile-fmul`) - multiplies two values then arithmetic-right-shifts
  the result by 16 to restore the binary point:

```vdu
(vp-mul-rr src dst)
(vp-asr-cr 16 dst)
```

- `</` (`compile-fdiv`) - left-shifts the dividend by 16 before dividing to
  preserve the binary point:

```vdu
(vp-shl-cr 16 dividend)
(vp-ext-rr dividend hi)
(vp-div-rrr divisor hi dividend)
```

The `fixed` type string character is `x`. The `real` (IEEE double) type string
character is `f`.

## Debug Output

Setting `*build_inst*` to `:t` before an `(assign)` call causes both the
pre-optimizer and post-optimizer instruction lists to be printed to standard
output:

```vdu
(let ((*build_inst* :t))
	(def-vars (int a b c))
	(push-scope)
	(assign {a + b} {c})
	(pop-scope)
	(return))
```

This is the primary mechanism for inspecting what code the CScript compiler
generates for a given expression.

## Summary of Key Source Locations

Tokenizer:

- File: `lib/asm/cscript.inc`
- Function: `cscript-tokenize`

Reverse polish conversion:

- File: `lib/asm/cscript.inc`
- Functions: `cscript-reverse-polish`

Main compile loop:

- File: `lib/asm/cscript.inc`
- Functions: `cscript-compile`, `cscript`

Virtual register stack:

- File: `lib/asm/cscript.inc`
- Symbols: `+vreg`, `push-reg`, `pop-reg`, `reset-reg-stack`

Type and deref machinery:

- File: `lib/asm/cscript.inc`
- Functions: `compile-deref`, `compile-deref?`, `pop-value`, `top-value`

Symbol lookup and referencing:

- File: `lib/asm/cscript.inc`
- Function: `compile-ref`

Store operator:

- File: `lib/asm/cscript.inc`
- Function: `compile-arrow`

Real register binding:

- File: `lib/asm/cscript.inc`
- Function: `def-reg-map`

Operator scope registry:

- File: `lib/asm/scopes.inc`
- Functions: `scope-operator`, `scope-get`

Peephole optimizer:

- File: `lib/asm/csopt.inc`
- Function: `opt-inst-list`

Assign dispatcher:

- File: `lib/asm/assign.inc`
- Function: `assign`

Script-to-Script assign:

- File: `lib/asm/assign.inc`
- Function: `assign-script-to-script`

Script-to-ASM assign:

- File: `lib/asm/assign.inc`
- Function: `assign-script-to-asm`

ASM-to-Script assign:

- File: `lib/asm/assign.inc`
- Function: `assign-asm-to-script`

ASM-to-ASM assign:

- File: `lib/asm/assign.inc`
- Function: `assign-asm-to-asm`

Topology sort for copy ordering:

- File: `lib/asm/assign.inc`
- Function: `assign-topology-sort`
